import React from 'react';

interface LabelTagsProps {
  style?: React.CSSProperties
  maxTagCount?: number
  data: {
    projName: string
  }[]
}
const TeamTags: React.FC<LabelTagsProps> = ({
  data, maxTagCount = 3, style, ...otherProps
}) => {
  if (!data || !data.length) {
    return <span>无</span>;
  }
  const visibleData = data.slice(0, maxTagCount);
  const hiddenData = data.slice(maxTagCount);
  const compact = data.length > maxTagCount;
  return (
    <div style={{ display: 'inline-flex', ...style }}>
      {data.map((team: any) => team.projName).join(' 、 ')}
    </div>
  );
};

export default TeamTags;
