import React from 'react';
import { Tag } from 'choerodon-ui';
import { Tooltip } from 'choerodon-ui/pro';

/**
 * 临近更新时间呈现
 * @param props => 更新时间信息
 * @returns React 函数式组件
 */
export function LastUpdateTime({ text }) {
  return (
    <Tooltip mouseEnterDelay={0.5} title={`日期： ${text}`}>
      <div
        style={{ minWidth: 50 }}
      >
        {text}
      </div>
    </Tooltip>
  );
}

/**
 * 冲刺呈现
 * @param props => 冲刺对象
 * @returns React 函数式组件
 */
export function Sprint({ objArray, name }) {
  if (objArray) {
    if (objArray.length > 0) {
      return (
        <div style={{
          display: 'inline-flex', overflow: 'hidden', textOverflow: 'ellipsis', width: '100%',
        }}
        >
          <Tag
            color="blue"
            style={{
              maxWidth: 160,
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
              cursor: 'auto',
            }}
          >
            {name}
          </Tag>
          { objArray.length > 1 ? <Tag color="blue">...</Tag> : null }
        </div>
      );
    }
  }
  return null;
}

/**
 * 冲刺呈现
 * @param props => 冲刺对象
 * @returns React 函数式组件
 */
export function Epic({ color, name }) {
  const style = {
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
}
