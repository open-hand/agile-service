import React from 'react';
import { Observer } from 'mobx-react-lite';
import Loading from '@/components/Loading';
import DetailContainer from '@/components/detail-container';
import Header from './components/header';
import Body from './components/body';
import { usePublishVersionContext } from './stores';
import styles from './Container.less';

const Container: React.FC = () => {
  const { store, detailProps } = usePublishVersionContext();
  return (
    <div className={styles.container}>
      <Observer>
        {() => {
          console.log('loading  <Loading loading={store.getLoading} />', store.getLoading);
          return <Loading loading={store.getLoading} />;
        }}
      </Observer>
      <Header />
      <Body />
      <DetailContainer {...detailProps} />
    </div>
  );
};

export default Container;
