import { FC, PropsWithChildren, createContext, useCallback, useContext, useEffect, useState } from "react";
import { Channel } from "../models/Channel";
import toast from "react-hot-toast";
import WebSocketContext from "./WebSocketContext";

interface IChannelContext {
    channels: Channel[]
    fetchChannels: () => void
}

const ChannelContext = createContext({} as IChannelContext)

export default ChannelContext

export const ChannelContextProvider: FC<PropsWithChildren> = ({ children }) => {
    const { connected } = useContext(WebSocketContext)

    const [channels, setChannels] = useState<Channel[]>([])

    const fetchChannels = useCallback(() => {
        fetch("http://localhost:8080/channels")
            .then(async response => {
                setChannels(await response.json())
            })
            .catch(e => {
                console.error(e)
                toast.error("Could not load channels")
            })
    }, [setChannels])

    useEffect(() => {
        if (connected) fetchChannels()
    }, [connected, fetchChannels])

    return (
        <ChannelContext.Provider value={{
            channels,
            fetchChannels
        }}>
            {children}
        </ChannelContext.Provider>
    )
}