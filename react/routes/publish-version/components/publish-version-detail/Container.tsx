import React from 'react';
import { Observer } from 'mobx-react-lite';
import Loading from '@/components/Loading';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { useReleaseDetailContext } from './stores';
import Header from './components/Header';
import Body from './components/Body';

const Container: React.FC = () => {
  const { store, prefixCls, detailProps } = useReleaseDetailContext();
  return (
    <div className={prefixCls}>
      <Observer>
        {() => <Loading loading={store.loading} />}
      </Observer>
      <Header />
      <Body />
      <DetailContainer {...detailProps} />
    </div>
  );
};

export default Container;
