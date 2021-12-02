import React, { useCallback } from 'react';
import { HeaderButtons } from '@choerodon/master';
import { observer } from 'mobx-react-lite';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import useFormatMessage from '@/hooks/useFormatMessage';

const ChartsButtons = () => {
  const handleClickBtn = useCallback(() => {
    to(LINK_URL.addCustomReport);
  }, []);
  const formatMessage = useFormatMessage('agile.chart');

  return (
    <HeaderButtons items={[{
      name: formatMessage({ id: 'custom.chart' }),
      icon: 'playlist_add',
      handler: handleClickBtn,
      display: true,
    }]}
    />
  );
};

export default observer(ChartsButtons);
