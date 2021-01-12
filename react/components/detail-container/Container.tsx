import React, { useRef, useEffect, useCallback } from 'react';
import ResizeAble from '@/components/ResizeAble';
import { find } from 'lodash';
import { Tooltip } from 'choerodon-ui/pro';
import EditIssue from '@/components/EditIssue';
import './Container.less';
import { useDetailContainerContext } from './context';
import Back from './back.svg';

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
}];
export function registerPath(route: Route) {
  if (!paths.find((p) => p.path === route.path)) {
    paths.push(route);
  }
}
const Container: React.FC = () => {
  const {
    outside, topAnnouncementHeight, match, routes, close, pop, push,
  } = useDetailContainerContext();
  const container = useRef<HTMLDivElement>(null);
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
    localStorage.setItem('agile.EditIssue.width', `${width}px`);
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
  return (
    <div
      className={prefixCls}
      style={{
        top: outside ? 75 : 50 + topAnnouncementHeight,
      }}
    >
      <ResizeAble
        modes={['left']}
        size={{
          maxWidth: window.innerWidth * 0.6,
          minWidth: 440,
        }}
        defaultSize={{
          width: localStorage.getItem('agile.EditIssue.width') || 640,
          height: '100%',
        }}
        onResizeEnd={handleResizeEnd}
        onResize={handleResize}
      >
        <div className={`${prefixCls}-resize`} ref={container}>
          {routes.length > 1 && (
            <Tooltip title="返回" placement="top">
              <div
                role="none"
                className={`${prefixCls}-back`}
                onClick={pop}
              >
                <img src={Back} alt="" />
              </div>
            </Tooltip>
          )}
          {match ? render() : null}
        </div>
      </ResizeAble>
    </div>
  );
};

export default Container;
