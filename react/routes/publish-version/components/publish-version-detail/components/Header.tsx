import { Button } from 'choerodon-ui/pro/lib';
import React from 'react';
import { observer } from 'mobx-react-lite';
import { useDetailContainerContext } from '@/components/detail-container/context';
import BaseTag from '@/components/tag/base-tag';
import VERSION_STATUS_TYPE from '@/constants/VERSION_STATUS_TYPE';
import { useReleaseDetailContext } from '../stores';
import Summary from './summary';
import './Header.less';

const Header: React.FC<{}> = () => {
  const { prefixCls, store, modal } = useReleaseDetailContext();
  const { statusCode, statusName } = store.getCurrentData;
  const { color, name } = VERSION_STATUS_TYPE[statusCode as keyof typeof VERSION_STATUS_TYPE] || {};
  return (
    <div className={`${prefixCls}-header`}>

      <div className={`${prefixCls}-header-line`}>
        <Summary />
        <BaseTag color={color} text={name} />

      </div>
      <Button
        onClick={() => {
          modal?.close();
        }}
        className={`${prefixCls}-header-btn`}
        icon="last_page"
      >
        隐藏详情
      </Button>
    </div>
  );
};
export default observer(Header);
