import { FC, useCallback, useContext, useEffect, useState } from "react";
import ChannelContext from "../../contexts/ChannelContext";
import WebSocketContext from "../../contexts/WebSocketContext";
import { MessageHelper } from "../../utils/MessageHelper";
import toast from "react-hot-toast";

const ChannelList: FC = () => {
    const [subscription, setSubscription] = useState<string[]>([])

    const { subscribe, cancelSubscription, messages, setId } = useContext(WebSocketContext)
    const { channels, fetchChannels } = useContext(ChannelContext)

    const onClickChannel = (id: string) => {
        let isSubscribed = subscription.some(sub => sub === id)

        if (isSubscribed) {
            setSubscription(prev => prev.filter(sub => sub !== id))
            cancelSubscription(id)
        } else {
            setSubscription(prev => [...prev, id])
            subscribe(id)
        }
    }

    const handleMessage = useCallback((msg: string | undefined) => {
        if (!msg) return

        switch (MessageHelper.getMessageType(msg)) {
            case "new_channel":
                toast.success("New channel")
                fetchChannels()

                break;
            case "id":
                setId(MessageHelper.getPayload(msg))
                fetchChannels()

                break;
        }
    }, [setId, fetchChannels])

    useEffect(() => handleMessage(messages[messages.length - 1]), [messages, handleMessage])

    return (
        <div className="ChannelList">
            <h1>Channels</h1>
            {
                channels.map(channel => {
                    let isSubscribed = subscription.some(sub => sub === channel.id)

                    return (
                        <div
                            className={`
                                ChannelList__Channel
                                ${isSubscribed && "ChannelList__Channel--subscribed"}
                            `}
                            key={channel.id}
                            onClick={() => onClickChannel(channel.id)}
                        >
                            <div className="ChannelList__ChannelSwitch">
                                <div className="ChannelList__ChannelButton"></div>
                            </div>
                            <p>{channel.name}</p>
                        </div>
                    )
                })
            }
        </div>
    )
}

export default ChannelList