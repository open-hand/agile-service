import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';

interface EpicTagProps {
  color: string
  name: string
}
const EpicTag: React.FC<EpicTagProps> = ({ color, name }) => {
  const style: React.CSSProperties = {
    color,
    borderWidth: '1px',
    borderStyle: 'solid',
    borderColor: color,
    borderRadius: '2px',
    fontSize: '13px',
    lineHeight: '20px',
    padding: '0 8px',
    display: 'inline-block',
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
    maxWidth: '100%',
  };
  return name ? <Tooltip mouseEnterDelay={0.5} title={name}><span style={style}>{name}</span></Tooltip> : null;
};
export default EpicTag;
