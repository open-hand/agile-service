import React, { useEffect, useMemo, useRef } from 'react';
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
  const maxWidth = useMemo(() => {
    const pageContentWidth = !size.width || !menuSize.width ? 0 : size.width - 58;
    const newWidth = pageContentWidth && size.height && size.height! > 80 ? pageContentWidth / 2 : pageContentWidth;
    console.log('newWidth:...', newWidth);
    return newWidth;
  }, [menuSize.width, size.width]);
  return (
    <div ref={ref} className={styles.fields}>
      <ReleaseDate />
      <LatestUpdateUser />
      <LatestUpdateDate />
      <CreateUser />
      <CreateDate />
      <Description maxWidth={maxWidth} />
    </div>
  );
}
export default observer(PublishVersionDetail);
