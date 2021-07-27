import React from 'react';
import { AnimationLoading as Loading } from '@choerodon/components';
import classNames from 'classnames';
import './AnimationLoading.less';

interface IAnimationLoadingProps {
    loading?: boolean
    className?: string
    style?: React.CSSProperties
    contentClassName?: string
    contentStyle?: React.CSSProperties
}
const AnimationLoading: React.FC<IAnimationLoadingProps> = ({
  loading, style, contentStyle, className, children, contentClassName,
}) => {
  const prefixCls = 'c7n-agile-animation-loading';
  return (
    <div className={classNames(prefixCls, className)} style={style}>
      <Loading display={loading} />
      <div style={{ visibility: loading ? 'hidden' : 'visible', ...contentStyle }} className={contentClassName}>
        {children}
      </div>
    </div>
  );
};
export default AnimationLoading;
