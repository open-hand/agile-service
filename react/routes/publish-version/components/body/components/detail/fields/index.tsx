import React, { useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { useSize } from 'ahooks';
import ReleaseDate from './release-date';
import LatestUpdateUser from './latest-update-user';
import LatestUpdateDate from './latest-update-date';
import CreateUser from './create-user';
import CreateDate from './create-date';
import Description from './description';
import styles from './index.less';

function PublishVersionDetail() {
  const ref = useRef(null);
  const size = useSize(ref);
  const menuSize = useSize(document.getElementById('menu'));
  return (
    <div ref={ref} className={styles.fields}>
      <ReleaseDate />
      <LatestUpdateUser />
      <LatestUpdateDate />
      <CreateUser />
      <CreateDate />
      <Description maxWidth={size.width && size.height && size.height > 80 ? size.width / 2 : size.width} />
    </div>
  );
}
export default observer(PublishVersionDetail);
