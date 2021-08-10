import classNames from 'classnames';
import { omit } from 'lodash';
import React, {
  forwardRef,
  useMemo, useCallback,
} from 'react';
import { useLoading } from './LoadingProvider';

interface ILoadingHiddenWrapProps {
  /** 关联的loadId，会自动检查关联的load是否加载完毕  */
  loadIds?: string[]

  // /** @default 'false' 保持子元素活跃  不卸载 */
  // keepActive?: boolean
  wrapClassName?: string
  wrapStyle?: React.CSSProperties
}
/**
 * loading进行中隐藏节点
 *
 */
const LoadingHiddenWrap: React.FC<ILoadingHiddenWrapProps> = forwardRef(({
  children, loadIds, wrapClassName, wrapStyle,
}, ref: any) => {
  const { loading, childrenLoadMap } = useLoading();
  const isHidden = useMemo(() => {
    if (loading) {
      return true;
    }
    if (loadIds?.length) {
      return !!loadIds.map((i) => {
        const ch = childrenLoadMap.get(i);
        return ch?.status === 'doing' || !ch?.finishInit;
      }).filter(Boolean).length;
    }
    return false;
  }, [childrenLoadMap, loadIds, loading]);
  const renderChildren = useCallback(() => {
    if (isHidden) {
      return null;
    }
    if (typeof (children) === 'function') {
      return children({ isHidden });
    }
    return children;
  }, [children, isHidden]);

  return (
    isHidden ? (
      <span className={wrapClassName} style={{ ...wrapStyle, visibility: 'hidden' }} ref={ref}>
        {renderChildren()}
      </span>
    ) : <>{renderChildren()}</>
  );
});
/**
 * 保持children不重复渲染
 * @param WrappedComponent
 * @returns
 */
function keepChildren(WrappedComponent: React.FC) {
  return class LoadingHiddenWrapHOC extends React.PureComponent<ILoadingHiddenWrapProps, any, any> {
    childrenWrapRef: React.RefObject<HTMLSpanElement> | null = null

    node: ChildNode[] = []

    constructor(props: ILoadingHiddenWrapProps) {
      super(props);
      this.state = { cacheChildren: null as any };
      this.childrenWrapRef = React.createRef<HTMLSpanElement>();
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
          if (preState.cacheChildren) {
            return preState.cacheChildren;
          }
          return { cacheChildren: React.cloneElement(<>{children}</>, { key: 'keepChildren' }) };
        });
      }
      return this.state.cacheChildren;
    }

    renderChildren({ isHidden }: { isHidden: boolean }) {
      if (isHidden && this.childrenWrapRef?.current && this.node.length === 0) {
        const firstChildren = this.childrenWrapRef?.current.childNodes;
        if (firstChildren) {
          const parent = this.childrenWrapRef?.current;
          firstChildren.forEach((item) => {
            const delChildren = parent.removeChild(item);
            this.node.push(delChildren);
          });
        }
      } else if (this.node.length > 0 && !isHidden && this.childrenWrapRef?.current) {
        let appendNode = this.node.pop();
        while (appendNode) {
          this.childrenWrapRef?.current.appendChild(appendNode);
          appendNode = this.node.pop();
        }
      }
      return this.getChildren();
    }

    render() {
      // if (!this.props.keepActive) {
      //   const { children } = this.props;
      //   return <WrappedComponent {...this.getOtherProps() as any} />;
      // }
      return (
        <span ref={this.childrenWrapRef} className={classNames('keep-active-node', this.props.wrapClassName)} style={this.props.wrapStyle}>
          <WrappedComponent {...this.getOtherProps()}>
            {({ isHidden }: any) => this.renderChildren({ isHidden })}
          </WrappedComponent>
        </span>

      );
    }
  };
}

export default LoadingHiddenWrap;
