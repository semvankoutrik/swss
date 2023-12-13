import React, { forwardRef, useMemo } from "react";
import MaterialIcon from "../MaterialIcon/MaterialIcon";

interface IProps extends React.DetailedHTMLProps<React.InputHTMLAttributes<HTMLInputElement>, HTMLInputElement> {
    label: string,
    buttonIcon: string,
    errors?: string[]
}

const FilledInput = forwardRef<HTMLInputElement, IProps>((
    {
        label,
        errors,
        buttonIcon,
        ...props
    },
    ref
) => {
    const baseClassName = "FilledInput"

    const isEmpty = useMemo(() => props.value === "" || (props.value !== "0" && !props.value), [props.value])

    return (
        <div className={`${baseClassName}__Container`}>
            <div className={`
                ${baseClassName}
                ${errors !== undefined && errors.length !== 0 && `${baseClassName}--error`}
                ${props.className ?? ""}
            `}>
                <input
                    ref={ref}
                    {...props}
                    type="text"
                    value={props.value ?? ""}
                    placeholder={undefined}
                    className={`
                    ${baseClassName}__Input 
                    ${isEmpty && `${baseClassName}__Input--empty`}
                `}
                    size={1}
                />

                <label className={`${baseClassName}__Label`}>{label}</label>

                <button className={`${baseClassName}__Button`}>
                    <MaterialIcon name={buttonIcon} />
                </button>
            </div>
            {
                errors &&
                errors.map(error => (
                    <p className={`${baseClassName}__Error`}>{error}</p>
                ))
            }
        </div>
    )
})

export default FilledInput