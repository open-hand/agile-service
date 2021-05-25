import React from 'react';

interface SprintTagProps {
  style?: React.CSSProperties
  data: {
    epicColor: string
    epicName: string
  }
}
const EpicTag: React.FC<SprintTagProps> = ({
  data, style, ...otherProps
}) => {
  if (!data) {
    return <span>无</span>;
  }

  return (
    <div
      style={{
        color: data.epicColor,
        borderWidth: '1px',
        borderStyle: 'solid',
        borderColor: data.epicColor,
        borderRadius: '2px',
        fontSize: '13px',
        lineHeight: '20px',
        padding: '0 8px',
        display: 'inline-block',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        ...style,
      }}
      {...otherProps}
    >
      {data.epicName}
    </div>
  );
};

export default EpicTag;
