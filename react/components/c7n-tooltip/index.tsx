import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import type { TooltipProps } from 'choerodon-ui/pro/lib/tooltip/Tooltip';
import isOverflow from 'choerodon-ui/pro/lib/overflow-tip/util';

import styles from './index.less';

const C7NTooltip = ({
  children = '', className = '', tooltipConfig = {}, ...otherProps
  // eslint-disable-next-line react/require-default-props
}: { children: string, className?: string, tooltipConfig?: TooltipProps }) => (
  <span
    className={`${styles.c7nTooltip} ${className}`}
    onMouseEnter={(e) => {
      const { target } = e;
      // @ts-ignore
      if (isOverflow(target)) {
        Tooltip.show(target, {
          title: children,
          placement: 'top',
          ...tooltipConfig,
        });
      }
    }}
    onMouseLeave={Tooltip.hide}
    {...otherProps}
  >
    {children}
  </span>
);

export default C7NTooltip;
