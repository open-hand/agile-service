import React from 'react';
import type { Gantt, GanttProps, GanttRef } from '@choerodon/gantt';
import {
  Tooltip, Icon,
} from 'choerodon-ui/pro';
import classNames from 'classnames';
import './index.less';

interface GanttExpandIconProps {
    collapsed?: boolean
    wrapElementProps?: React.HTMLAttributes<HTMLDivElement>
}
const GanttExpandIcon: React.FC<GanttExpandIconProps> = ({ collapsed, wrapElementProps }) => (
  <div
    role="none"
    {...wrapElementProps}
    className={classNames('c7n-gantt-expand-icon', {
      'c7n-gantt-expand-icon-expanded': !collapsed,
    }, wrapElementProps?.className)}
  >
    <Icon type="navigate_next" />
  </div>
);
GanttExpandIcon.defaultProps = {
  collapsed: undefined,
  wrapElementProps: {},
};
export default GanttExpandIcon;
