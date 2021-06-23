import React, { useCallback } from 'react';
import { HeaderButtons } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';

const ChartsButtons = () => {
  const handleClickBtn = useCallback(() => {
    to(LINK_URL.addCustomReport);
  }, []);
  return (
    <HeaderButtons items={[{
      name: '自定义报表',
      icon: 'playlist_add',
      handler: handleClickBtn,
      display: true,
    }]}
    />
  );
};

export default observer(ChartsButtons);
