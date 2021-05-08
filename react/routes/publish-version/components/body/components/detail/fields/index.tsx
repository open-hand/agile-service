import React from 'react';
import {
  TabPage as Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import {
  Button, Menu, Table, Tooltip, Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  omit,
} from 'lodash';
import './PublishVersion.less';
import ReleaseDate from './release-date';

const { Column } = Table;

function PublishVersionDetail() {
//   const { prefixCls, tableDataSet } = usePublishVersionContext();

  return (
    <>
      <ReleaseDate />
    </>
  );
}
export default observer(PublishVersionDetail);
