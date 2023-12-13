import { FC, PropsWithChildren } from "react";
import { WebSocketContextProvider } from "./WebSocketContext";
import { ChannelContextProvider } from "./ChannelContext";

export const Contexts: FC<PropsWithChildren> = ({ children }) => {
    return (
        <WebSocketContextProvider>
            <ChannelContextProvider>
                {children}
            </ChannelContextProvider>
        </WebSocketContextProvider>
    )
}