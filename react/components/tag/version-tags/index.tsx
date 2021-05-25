import { map } from 'lodash';
import React from 'react';

interface LabelTagsProps {
  style?: React.CSSProperties
  data: {
    labelName: string
  }[]
}
const VersionTags: React.FC<LabelTagsProps> = ({
  data, style, ...otherProps
}) => {
  if (!data || !data.length) {
    return <span>无</span>;
  }

  return (
    <div className="primary" style={{ wordBreak: 'break-word', ...style }}>
      {map(data, 'name').join(' , ')}
    </div>
  );
};

export default VersionTags;
