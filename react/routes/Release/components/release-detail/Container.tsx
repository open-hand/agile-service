import React from 'react';
import { Observer } from 'mobx-react-lite';
import Loading from '@/components/Loading';
import { useReleaseDetailContext } from './stores';
import Header from './components/Header';
import Body from './components/Body';

const Container: React.FC = () => {
  const { store } = useReleaseDetailContext();
  return (
    <>
      <Observer>
        {() => <Loading loading={store.loading} />}
      </Observer>
      <Header />
      <Body />
    </>
  );
};

export default Container;
