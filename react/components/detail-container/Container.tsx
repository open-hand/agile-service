import React, { useRef, useEffect, useCallback } from 'react';
import ResizeAble from '@/components/ResizeAble';
import { find } from 'lodash';
import styles from './Container.less';
import { useDetailContainerContext } from './context';

const Issue = () => {
  const { push } = useDetailContainerContext();
  return (
    <div>
      issue
      <button
        onClick={() => {
          push('demand');
        }}
      >
        push
      </button>
    </div>
  );
};
const Demand = () => {
  const { push } = useDetailContainerContext();
  return (
    <div>
      Demand
      <button
        onClick={() => {
          push('issue');
        }}
      >
        push
      </button>
    </div>
  );
};
const paths: {
  path: string,
  component: React.ComponentType<any>
}[] = [{
  path: 'issue',
  component: Issue,
}, {
  path: 'demand',
  component: Demand,
}];
const Container: React.FC = ({ children }) => {
  const {
    outside, topAnnouncementHeight, match, routes, close, pop,
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
  console.log(match);
  const render = useCallback(() => {
    const target = find(paths, { path: match });
    if (target) {
      // @ts-ignore
      return React.createElement(target.component);
    }
    return null;
  }, [match]);
  return (
    <div
      className={styles.container}
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
        <div className={styles.resize} ref={container}>
          {match ? render() : children}
          {routes.length > 1 && (
          <button
            onClick={pop}
          >
            返回
          </button>
          )}

          <button
            onClick={close}
          >
            关闭
          </button>
        </div>
      </ResizeAble>
    </div>
  );
};

export default Container;
