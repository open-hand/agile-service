import React from 'react';
import { Icon, Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { omit } from 'lodash';
import { style } from 'dom-helpers';
import styles from './index.less';

interface Props extends React.HTMLAttributes<HTMLDivElement> {
  active: boolean
  disabled?: boolean
  tooltip?: boolean
  activeTooltip?: string
  inActiveTooltip?: string
}

const Star: React.FC<Props> = ({
  active,
  disabled,
  className: propsClassName,
  tooltip = true,
  activeTooltip = '',
  inActiveTooltip = '',
  ...otherProps
}) => {
  // 禁用并且没关注，不显示
  if (disabled && !active) {
    return null;
  }
  const className = classNames({
    [styles.star]: true,
    [styles.star_active]: active,
    propsClassName,
  });

  const star = (
    <Icon
      className={className}
      type={active ? 'stars' : 'star_border'}
      {...omit(otherProps, 'onClick', 'style')}
      onClick={disabled ? undefined : otherProps.onClick}
      style={{
        cursor: disabled ? 'auto' : 'pointer',
        ...otherProps.style,
      }}
    />
  );
  if (disabled) {
    return star;
  }
  return tooltip ? (
    <Tooltip title={active ? activeTooltip : inActiveTooltip}>
      {star}
    </Tooltip>
  ) : star;
};
export default Star;
