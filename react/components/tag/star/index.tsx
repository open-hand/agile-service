import React from 'react';
import { Icon, Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import { omit } from 'lodash';
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
  const className = classNames({
    [styles.star]: true,
    [styles.star_active]: active,
    propsClassName,
  });

  const star = (
    <Icon
      className={className}
      type="star_border"
      {...omit(otherProps, 'onClick')}
      onClick={disabled ? undefined : otherProps.onClick}
      style={{
        cursor: disabled ? 'auto' : 'pointer',
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
