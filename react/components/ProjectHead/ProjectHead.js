import React, { memo } from 'react';
import { Tooltip } from 'choerodon-ui';

function getFirst(str) {
  if (!str) {
    return '';
  }
  const re = /[\u4E00-\u9FA5]/g;
  for (let i = 0, len = str.length; i < len; i += 1) {
    if (re.test(str[i])) {
      return str[i];
    }
  }
  return str[0];
}
const ProjectHead = memo(({
  project,
  color,
  size,
  hiddenText,
  style,
  className = '',
  type,
  tooltip = true,
  title,
  ...restProps
}) => {
  const iconSize = size || 18;
  const {
    id, avatar, imageUrl, name,
  } = project;
  const img = avatar || imageUrl;
  const renderTooltip = () => {
    if (title) {
      return title;
    }
    if (name) {
      return name;
    }
  };

  const renderContent = () => (
    <div
      className={`c7n-program-projectHead ${className}`}
      style={{        
        display: id ? 'flex' : 'none',
        maxWidth: 108,
        ...style,
      }}
      {...restProps}
    >
      <div
        style={{
          width: iconSize,
          height: iconSize,
          background: '#c5cbe8',
          color: '#6473c3',
          overflow: 'hidden',
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          marginRight: 5,
          textAlign: 'center',
          borderRadius: '50%',
          flexShrink: 0,
        }}
      >
        {
          img ? (
            <img src={img} alt="" style={{ width: iconSize, height: iconSize }} />
          ) : (
            <span style={{
              width: iconSize, height: iconSize, lineHeight: `${iconSize}px`, textAlign: 'center', color: '#6473c3',
            }}
            >
              {getFirst(name)}
            </span>
          )
        }
      </div>
      {
        hiddenText ? null : (
          <span
            style={{
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              // fontSize: '13px',
              lineHeight: `${iconSize}px`,
              color: color || 'var(--text-color3)',
            }}
          >
            {name}
          </span>
        )
      }
    </div>
  );
  return tooltip
    ? (
      <Tooltip title={renderTooltip()} mouseEnterDelay={0.5}>
        {renderContent()}
      </Tooltip>
    )
    : renderContent();
});
export default ProjectHead;
