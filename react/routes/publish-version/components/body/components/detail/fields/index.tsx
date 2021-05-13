import React from 'react';
import { observer } from 'mobx-react-lite';
import ReleaseDate from './release-date';
import LatestUpdateUser from './latest-update-user';
import LatestUpdateDate from './latest-update-date';
import CreateUser from './create-user';
import CreateDate from './create-date';
import Description from './description';
import styles from './index.less';

function PublishVersionDetail() {
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
