import React, { useRef, useEffect, useCallback } from 'react';
import ResizeAble from '@/components/ResizeAble';
import { find } from 'lodash';
import { Button } from 'choerodon-ui/pro';
import IssueDetail from '@/components/IssueDetail';
import EditIssue from '@/components/EditIssue';
import styles from './Container.less';
import { useDetailContainerContext } from './context';

const Issue = () => {
  const { push } = useDetailContainerContext();
  return (
    <div>
      issue
      <Button
        onClick={() => {
          push({ path: 'demand' });
        }}
      >
        push
      </Button>
    </div>
  );
};
const Demand = () => {
  const { push, close } = useDetailContainerContext();
  return (
    <div>
      Demand
      <Button
        onClick={() => {
          push({ path: 'issue' });
        }}
      >
        push
      </Button>
      <Button
        onClick={() => {
          close();
        }}
      >
        关闭
      </Button>
    </div>
  );
};
const paths: {
  path: string,
  component: React.ComponentType<any>
}[] = [{
  path: 'issue',
  component: EditIssue,
}, {
  path: 'demand',
  component: Demand,
}];
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
      return React.createElement(target.component, match.props);
    }
    return null;
  }, [match.path, match.props]);
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
          {routes.length > 1 && (
          <Button
            onClick={pop}
          >
            返回
          </Button>
          )}
          {match ? render() : null}
        </div>
      </ResizeAble>
    </div>
  );
};

export default Container;
