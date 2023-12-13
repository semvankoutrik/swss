export class MessageHelper {
    static setName(name: string) {
        return `set_name/${name}`
    }

    static subscribe(id: string) {
        return `sub/${id}`
    }

    static cancelSubscription(id: string) {
        return `cancel_sub/${id}`
    }

    static getMessageType(msg: string) {
        return msg.split("/")[0]
    }

    static getPayload(msg: string) {
        return msg.split("/")[1]
    }

    static getPubChannel(msg: string) {
        return this.getPayload(msg).split(":")[0]
    }

    static getPubMessage(msg: string) {
        return this.getPayload(msg).split(":")[1]
    }
}