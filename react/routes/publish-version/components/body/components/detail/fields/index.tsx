import React, { useMemo, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { useSize } from 'ahooks';
import { mean } from 'lodash';
import moment from 'moment';
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
  const historyMaxWidth = useMemo(() => [] as Array<{ timeStamp: number, width: number }>, []);
  const menuSize = useSize(document.getElementById('menu'));
  const maxWidth = useMemo(() => {
    const newWidth = size.width && size.height && size.height > 80 ? size.width / 2 : (size.width || 0) - (menuSize.width || 0);
    const lastWidth = historyMaxWidth.pop();
    if (historyMaxWidth.length > 10) {
      historyMaxWidth.length = 0;
    }
    const newHistory = { timeStamp: moment().unix(), width: newWidth };
    historyMaxWidth.push(newHistory);
    if (lastWidth && (Math.abs(lastWidth.width - newWidth) < 15 || (newHistory.timeStamp - lastWidth.timeStamp < 1))) {
      historyMaxWidth.push(lastWidth);
      return lastWidth.width;
    }
    return newWidth;
  }, [historyMaxWidth, menuSize.width, size.height, size.width]);
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
