import React, { useRef, useEffect } from 'react';
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
  //   const ref = useRef<any>(null);
  //   useEffect(() => {
  //     if (!loading && ref.current) {
  //     //   ReactDom.render(ref.current.children, containerRef.current);
  //       console.log('ref...', ref.current.children);
  //     }
  //   }, [loading]);
  return loading ? (
    <div className={classNames(prefixCls, className)} style={style}>
      <Loading display={loading} />
      {/* {ref.curren} */}
      <div style={{ visibility: loading ? 'hidden' : 'visible', ...contentStyle }} className={contentClassName}>
        {children}
      </div>
    </div>
  ) : <>{children}</>;
};
export default AnimationLoading;
