import { FC, PropsWithChildren, createContext, useCallback, useEffect, useState } from "react";
import { MessageHelper } from "../utils/MessageHelper";
import toast from "react-hot-toast";

interface IWebSocketContext {
    connect: (username: string) => void
    subscribe: (id: string) => void
    cancelSubscription: (id: string) => void | undefined
    connected: boolean
    messages: string[]
    id: string | undefined
    setId: (id: string) => void
    name: string | undefined
}

const WebSocketContext = createContext({} as IWebSocketContext)

export default WebSocketContext

export const WebSocketContextProvider: FC<PropsWithChildren> = ({ children }) => {
    const [webSocket, setWebSocket] = useState<WebSocket>()
    const [name, setName] = useState<string>()
    const [id, setId] = useState<string>()
    const [messages, setMessages] = useState<string[]>([])

    const connect = (channelName: string) => {
        let ws = new WebSocket("ws://localhost:8080/ws")

        setWebSocket(ws)
        setName(channelName)
    }

    const subscribe = (id: string) => webSocket?.send(`sub/${id}`)
    const cancelSubscription = (id: string) => webSocket?.send(`cancel_sub/${id}`)

    const handleMessage = useCallback((event: MessageEvent) => {
        setMessages(prev => ([...prev, event.data]))
    }, [setMessages])

    const handleOpen = useCallback((_event: Event) => {
        toast.success("Connected")

        if (name) webSocket?.send(MessageHelper.setName(name))
    }, [webSocket, name])

    const handleClose = useCallback((event: CloseEvent) => {
        setWebSocket(undefined)

        switch (event.code) {
            case 1006:
                toast.error("Connection closed")

                break;
        }
    }, [])

    useEffect(() => {
        webSocket?.addEventListener("open", handleOpen)
        webSocket?.addEventListener("close", handleClose)
        webSocket?.addEventListener("message", handleMessage)

        return () => {
            webSocket?.removeEventListener("open", handleOpen)
            webSocket?.removeEventListener("close", handleClose)
            webSocket?.removeEventListener("message", handleMessage)
        }
    }, [
        webSocket,
        handleOpen,
        handleClose,
        handleMessage
    ])

    return (
        <WebSocketContext.Provider value={{
            connect,
            subscribe,
            cancelSubscription,
            connected: !!webSocket && webSocket.OPEN === 1,
            messages,
            id,
            setId,
            name
        }}>
            {children}
        </WebSocketContext.Provider>
    )
}