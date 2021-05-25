import React from 'react';

interface SprintTagProps {
  style?: React.CSSProperties
  data: {
    summary: string
  }
}
const FeatureTag: React.FC<SprintTagProps> = ({
  data, style, ...otherProps
}) => {
  if (!data) {
    return <span>无</span>;
  }

  return (
    <div
      className="primary"
      style={{
        wordBreak: 'break-word',
        ...style,
      }}
      {...otherProps}
    >
      {data.summary}
    </div>
  );
};

export default FeatureTag;
