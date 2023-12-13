import { FC, useMemo } from "react";

interface IProps {
    name: string
    className?: string
    type?: "regular" | "outlined" | "round" | "sharp" | "two-tone"
}

const MaterialIcon: FC<IProps> = ({ name, className, type }) => {
    const classes = useMemo(() => {
        const base = className ? `material-icon ${className}` : `material-icon`
        const modifier = `material-icon--${type ?? "regular"}`

        return `${base} ${modifier}`
    }, [className, type])

    return (
        <span className={classes}>{name}</span>
    )
}

export default MaterialIcon