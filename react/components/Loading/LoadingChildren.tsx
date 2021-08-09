import React, {
  useState, useMemo,
  useEffect,
} from 'react';
import { AnimationLoading as OriginAnimationLoading } from '@choerodon/components';
import classNames from 'classnames';

import {
  uniqueId,
} from 'lodash';
import {
  useMount, useUnmount,
} from 'ahooks';
import {
  ILoadingProps, IAnimationLoadingProps,
} from './type';
import { useLoading } from './LoadingProvider';
import './LoadingChildren.less';

const prefixCls = 'c7n-agile-animation-loading';

/**
 * 小鱼吐泡泡Loading
 */
const Loading: React.FC<ILoadingProps> = ({
  children, loading: propsLoading, noDeliverLoading, loadId: propsLoadId, allowSelfLoading, className: propsClassName, globalLoading, ...otherProps
}) => {
  const [loading, setLoading] = useState<boolean | undefined>(); /** 自身的loading */
  const {
    registerChildren, cancelRegisterChildren, isHasProvider, change,
  } = useLoading();

  const loadId = useMemo(() => propsLoadId || uniqueId('loading'), [propsLoadId]);
  useMount(() => {
    const status = propsLoading ? 'doing' : 'ready';
    !noDeliverLoading && registerChildren({
      loadId, changeLoading: setLoading, initStatus: propsLoading === undefined ? 'init' : status, allowSelfLoading,
    });
  });
  useUnmount(() => {
    cancelRegisterChildren(loadId);
  });

  useEffect(() => {
    // 兼容过往使用loading方法的 effect
    if (propsLoading !== undefined) {
      console.log(`Loading:${loadId} USE:${isHasProvider && !noDeliverLoading ? 'change' : 'setLoading'}`, propsLoading);
      isHasProvider && !noDeliverLoading ? change(loadId, !!propsLoading) : setLoading(propsLoading);
    }
  }, [change, isHasProvider, loadId, noDeliverLoading, propsLoading]);
  return (
    <AnimationLoading
      loading={!!loading}
      className={classNames({
        [`${prefixCls}-global-loading`]: globalLoading && loading,
        // [`${prefixCls}-global-loaded`]: !globalLoading,
      }, propsClassName)}
      {...otherProps}
    >
      {children}
    </AnimationLoading>
  );
};

/**
 * 动画Loading
 */
const AnimationLoading: React.FC<IAnimationLoadingProps> = ({
  loading, className, children, style, loadingStyle,
}) => (
  <div
    className={classNames(prefixCls, {
      [`${prefixCls}-no-children`]: !children,
      [`${prefixCls}-no-children-hidden`]: !children && !loading,
    }, className)}
    style={{ ...(loading ? loadingStyle : {}), ...style }}
  >
    <OriginAnimationLoading display={loading} className={classNames(`${prefixCls}-container`, { [`${prefixCls}-hidden`]: !loading })} />
    {children}
  </div>
);
export default Loading;
