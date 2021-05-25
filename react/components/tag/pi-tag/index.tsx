import React from 'react';

interface SprintTagProps {
  style?: React.CSSProperties
  data: {
    code: string
    name: string
  }
}
const PiTag: React.FC<SprintTagProps> = ({
  data, style, ...otherProps
}) => {
  if (!data) {
    return <span>无</span>;
  }

  return (
    <div
      style={{
        color: '#4d90fe',
        fontSize: '13px',
        lineHeight: '20px',
        display: 'inline-block',
        ...style,
      }}
      {...otherProps}
    >
      {`${data.code}-${data.name}`}
    </div>
  );
};

export default PiTag;
