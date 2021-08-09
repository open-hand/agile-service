import { omit } from 'lodash';
import React, {
  Fragment,
  useMemo,
} from 'react';
import { useLoading } from './LoadingProvider';
/**
 * loading进行中隐藏节点
 *
 *
 */
const LoadingHiddenWrap: React.FC<{ loadIds?: string[] }> = ({ children, loadIds }) => {
  const { loading, childrenLoadMap } = useLoading();
  const isHidden = useMemo(() => {
    if (loading) {
      return true;
    }
    if (loadIds?.length) {
      return !!loadIds.map((i) => childrenLoadMap.get(i)?.status === 'doing').filter(Boolean).length;
    }
    return false;
  }, [childrenLoadMap, loadIds, loading]);
  if (typeof (children) === 'function') {
    return children({ isHidden });
  }
  return (
    isHidden ? (
      <span style={{ visibility: 'hidden' }}>
        {children}
      </span>
    ) : <>{children}</>
  );
};
/**
 * 保持children不重复渲染
 * @param WrappedComponent
 * @returns
 */
function keepChildren(WrappedComponent: React.FC) {
  return class LoadingHiddenWrapHOC extends React.PureComponent<any, any> {
    constructor(props: any) {
      super(props);
      this.state = { cacheChildren: null as any };
    }

    getOtherProps() {
      return omit(this.props, 'children');
    }

    componentDidMount() {
      // this.setState({ cacheChildren: null });
    }

    getChildren() {
      if (!this.state.cacheChildren) {
        this.setState((preState: any = {}, preProps: any) => {
          const { children } = preProps;
          console.log('set.....', preState);
          if (preState.cacheChildren) {
            return preState.cacheChildren;
          }
          return { cacheChildren: React.cloneElement(<>{children}</>, { key: 'keepChildren' }) };
        });
      }
      return this.state.cacheChildren;
    }

    renderChildren({ isHidden }: { isHidden: boolean }) {
      console.log('isHidden...', isHidden);
      return this.getChildren();
    }

    render() {
      return (
        <WrappedComponent {...this.getOtherProps()}>
          {({ isHidden }: any) => this.renderChildren({ isHidden })}
        </WrappedComponent>
      );
    }
  };
}
export default LoadingHiddenWrap;
