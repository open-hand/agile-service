import React, { memo } from 'react';
import { Icon, Tooltip } from 'choerodon-ui/pro';
import './TypeTag.less';
import classNames from 'classnames';
import { IIssueType, IFeatureType } from '@/common/types';

interface Props {
  data: IIssueType
  /** 是否开启tooltip 手动传入tooltip内容 */
  tooltip?: boolean | string
  showName?: boolean
  className?: string
  style?: React.CSSProperties
  featureType?: IFeatureType
  iconSize?: number
}
const TypeTag: React.FC<Props> = ({
  data, showName, style, featureType, iconSize = 24, className, tooltip = true,
}) => {
  let {
    colour, name = '', icon,
  } = data || {};
  if (featureType === 'business') {
    colour = '#3D5AFE';
    name = '特性';
    icon = 'characteristic';
  } else if (featureType === 'enabler') {
    colour = '#FFCA28';
    name = '使能';
    // icon = 'characteristic';
  }
  if (icon === 'agile-backlog') {
    icon = 'highlight';
  }
  const reverse = ['agile_activity', 'agile_milestone', 'agile_view_timeline', 'agile_epic', 'agile_story', 'agile_fault', 'agile_task', 'agile_subtask', 'test-case', 'test-automation', 'agile-feature', 'agile_risk'].includes(icon);
  const tooltipTitle = typeof tooltip === 'boolean' && tooltip ? name : tooltip;
  return (
    <Tooltip
    // 用以点击外部创建进行匹配过滤使用
      popupClassName="c7n-agile-type_tag_popup-tip"
      title={tooltipTitle}
    >
      <div className={classNames('c7n-typeTag', className)} style={style}>
        {!reverse ? (
          <Icon
            className="c7n-typeTag-icon-normal"
            style={{
              transition: 'none',
              fontSize: iconSize * 15 / 24 || '15px',
              width: ((iconSize + 5) * 15 / 24) || '20px',
              height: ((iconSize + 5) * 15 / 24) || '20px',
              lineHeight: `${((iconSize + 5) * 15 / 24)}px` || '20px',
              background: colour || '#fab614',
              color: 'white',
            }}
            type={icon}
          />
        ) : (
          <Icon
            style={{
              transition: 'none',
              fontSize: iconSize || '24px',
              color: colour || '#fab614',
            }}
            type={icon || 'help'}
          />
        )}
        {
          showName && (
            <span className="name">{name}</span>
          )
        }
      </div>
    </Tooltip>
  );
};
export default memo(TypeTag);
