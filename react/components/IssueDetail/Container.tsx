import React, { useRef, useEffect } from 'react';
import { Observer } from 'mobx-react-lite';
import ResizeAble from '@/components/ResizeAble';
import Loading from '@/components/Loading';
import Header from './components/header';
import Body from './components/body';
import styles from './Container.less';
import { useDetailContext } from './context';

const Container: React.FC = () => {
  const { store, outside, topAnnouncementHeight } = useDetailContext();
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
          <Observer>
            {() => <Loading loading={store.loading} />}
          </Observer>
          <Header />
          <Body />
        </div>
      </ResizeAble>
    </div>
  );
};

export default Container;
