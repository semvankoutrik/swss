import { FC, FormEvent, useContext, useMemo, useState } from "react";
import FilledInput from "../FilledInput/FilledInput";
import WebSocketContext from "../../contexts/WebSocketContext";
import ChannelContext from "../../contexts/ChannelContext";
import { MessageHelper } from "../../utils/MessageHelper";

const Inbox: FC = () => {
    const { id, messages, name } = useContext(WebSocketContext)
    const { channels } = useContext(ChannelContext)

    const [message, setMessage] = useState<string>("")

    const onSubmit = (e: FormEvent) => {
        e.preventDefault()

        fetch(`http://localhost:8080/channels/${id}`, { method: "POST", body: message })
            .then(_ => {
                setMessage("")
            })
            .catch((e: any) => {
                console.error(e)
            })
    }

    const channelMessages = useMemo(() => {
        return messages
            .filter(m => MessageHelper.getMessageType(m) === "pub")
            .filter(m => {
                let channelId = MessageHelper.getPubChannel(m)
                let channel = channels.find(c => c.id === channelId)?.name

                return !!channel
            })
            .map(m => {
                let channelId = MessageHelper.getPubChannel(m)
                let channel = channels.find(c => c.id === channelId)?.name
                let message = MessageHelper.getPubMessage(m)

                return {
                    channelId,
                    channel,
                    message
                }
            })
    }, [messages, channels])

    return (
        <div className="Inbox">
            <div className="Inbox__Messages">
                {
                    channelMessages.map(m => {
                        return (
                            <div className="Inbox__Message">
                                <div className="Inbox__MessageHeader">
                                    <p className="Inbox__ChannelName">{m.channel ?? ""}</p>
                                    <p className="Inbox__ChannelId">{m.channelId}</p>
                                </div>
                                <p className="Inbox__MessageText">{m.message}</p>
                            </div>
                        )
                    })
                }
            </div>
            <form className="Inbox__Send" onSubmit={onSubmit}>
                <p className="Inbox__SendingInNameOf"><i>Send message to "{name}"</i></p>
                <FilledInput
                    label="Message"
                    className="Inbox__Input"
                    value={message}
                    onChange={(e) => setMessage(e.target.value)}
                    buttonIcon="send"
                />
            </form>
        </div>
    )
}

export default Inbox