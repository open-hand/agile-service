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
// import './PublishVersion.less';
import ReleaseDate from './release-date';
import LatestUpdateUser from './latest-update-user';
import LatestUpdateDate from './latest-update-date';
import CreateUser from './create-user';
import CreateDate from './create-date';
import Description from './description';
import styles from './index.less';

const { Column } = Table;

function PublishVersionDetail() {
//   const { prefixCls, tableDataSet } = usePublishVersionContext();

  return (
    <div className={styles.fields}>
      <ReleaseDate />
      <LatestUpdateUser />
      <LatestUpdateDate />
      <CreateUser />
      <CreateDate />
      <Description />
    </div>
  );
}
export default observer(PublishVersionDetail);
