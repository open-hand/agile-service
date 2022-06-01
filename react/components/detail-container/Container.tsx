import React, { useRef, useEffect, useCallback } from 'react';
import { find } from 'lodash';
import { Icon } from 'choerodon-ui/pro';
import ResizeAble from '@/components/ResizeAble';
import EditIssue from '@/components/EditIssue';
import './Container.less';
import { useDetailContainerContext } from './context';

const prefixCls = 'c7nagile-detail-container';
interface Route {
  path: string,
  component: React.ComponentType<any>
}
const paths: Route[] = [{
  path: 'issue',
  component: EditIssue,
}, {
  path: 'program_issue',
  component: EditIssue,
},
];
export function registerPath(route: Route) {
  if (!paths.find((p) => p.path === route.path)) {
    paths.push(route);
  }
}
function getStoredWidth() {
  const stored = localStorage.getItem('agile.EditIssue.width');
  if (stored === null) {
    return undefined;
  }
  const width = Number(stored);
  return Number.isNaN(width) ? undefined : width;
}
const Container: React.FC = () => {
  const {
    outside, topAnnouncementHeight, match, routes, close, pop, push, fullPage, resizeRef, filePreview, disableResizeWidth, noBorder = null,
  } = useDetailContainerContext();
  const container = useRef<HTMLDivElement>(null);
  const maxWidth = window.innerWidth * 0.6;
  const minWidth = 440;
  const defaultWidth = Math.max(minWidth, Math.min(maxWidth, getStoredWidth() ?? 640));
  useEffect(() => {
    setQuery();
  }, []);
  const setQuery = (width = container.current ? container.current.clientWidth : 0) => {
    if (container.current) {
      if (width <= 600) {
        container.current.setAttribute('max-width', '600px');
      } else {
        container.current.removeAttribute('max-width');
      }
    }
  };
  // @ts-ignore
  const handleResizeEnd = ({ width }) => {
    localStorage.setItem('agile.EditIssue.width', width);
  };
  // @ts-ignore
  const handleResize = ({ width }) => {
    setQuery(width);
  };
  const render = useCallback(() => {
    const target = find(paths, { path: match.path });
    if (target) {
      // @ts-ignore
      return React.createElement(target.component, { ...match.props, key: match.key });
    }
    return null;
  }, [match.key, match.path, match.props]);
  const boundaryElement = filePreview ? <div className={`${prefixCls}-border`} /> : <div className={`${prefixCls}-divider`} />;
  const leftElement = disableResizeWidth && (noBorder === null || noBorder) ? <></> : boundaryElement;
  const element = (
    <div
      className={`${prefixCls}-resize`}
      ref={container}
      style={{
        paddingTop: routes.length > 1 ? 34 : 0,
      }}
    >
      {leftElement}
      {routes.length > 1 && (
        <div
          role="none"
          className={`${prefixCls}-back`}
          onClick={pop}
        >
          <Icon type="navigate_before" />
          返回上一层
        </div>
      )}
      {match ? render() : null}
    </div>
  );

  return fullPage ? element : (
    <div
      className={prefixCls}
      style={{
        top: outside ? 75 : `calc(50px + ${topAnnouncementHeight})`,
      }}
    >
      <ResizeAble
        disabled={!!filePreview || disableResizeWidth}
        ref={resizeRef}
        modes={['left']}
        size={{
          maxWidth,
          minWidth,
        }}
        defaultSize={{
          width: filePreview ? 440 : defaultWidth,
          height: '100%',
        }}
        onResizeEnd={handleResizeEnd}
        onResize={handleResize}
      >
        {element}
      </ResizeAble>
    </div>
  );
};

export default Container;
