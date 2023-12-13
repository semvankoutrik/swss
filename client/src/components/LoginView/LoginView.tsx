import { FC, FormEvent, useContext, useState } from "react";
import FilledInput from "../FilledInput/FilledInput";
import WebSocketContext from "../../contexts/WebSocketContext";

const LoginView: FC = () => {
    const { connect } = useContext(WebSocketContext)

    const [name, setName] = useState<string>("")

    const onSubmit = (e: FormEvent) => {
        e.preventDefault()
        connect(name)
    }

    return (
        <div className="LoginView">
            <form
                onSubmit={onSubmit}
                className="LoginView__Form"
            >
                <FilledInput
                    label="Channel name"
                    className="LoginView__Input"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    buttonIcon="arrow_forward"
                />
            </form>
        </div>
    )
}

export default LoginView