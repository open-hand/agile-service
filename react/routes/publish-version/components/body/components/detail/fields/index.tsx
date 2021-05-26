import React, { useEffect, useMemo, useRef } from 'react';
import { observer, useObserver } from 'mobx-react-lite';
import { isEmpty } from 'lodash';
import { useSize } from 'ahooks';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import { IPublishVersionData } from '@/api';
import UserTag from '@/components/tag/user-tag';
import ReleaseDate from './release-date';
import LatestUpdateUser from './latest-update-user';
import LatestUpdateDate from './latest-update-date';
import CreateUser from './create-user';
import CreateDate from './create-date';
import Description from './description';
import styles from './index.less';
import Field from './field';

function getFieldContent(data: any, type?: string) {
  if (type === 'user') {
    return typeof (data) === 'object' ? <UserTag data={data} /> : '无';
  } if (type === 'date') {
    return !isEmpty(data) ? String(data).split(' ')[0] : '无';
  }
  return data;
}
function PublishVersionDetail({ customFields }: { customFields?: any[] }) {
  const ref = useRef(null);
  const size = useSize(ref);
  const { store } = usePublishVersionContext();
  const menuSize = useSize(document.getElementById('menu'));
  const maxWidth = useMemo(() => {
    const pageContentWidth = !size.width || !menuSize.width ? 0 : size.width - 58;
    const newWidth = pageContentWidth && size.height && size.height! > 80 ? pageContentWidth / 2 : pageContentWidth;
    console.log('newWidth:...', newWidth);
    return newWidth;
  }, [menuSize.width, size.width]);
  const fields = useObserver(() => {
    if (customFields) {
      return customFields.map((item) => {
        let { content } = item;
        if (item.dataKey === 'description') {
          return <Description maxWidth={maxWidth} />;
        }
        if (!item.content) {
          content = store.getCurrentData ? getFieldContent(store.getCurrentData[item.dataKey as keyof IPublishVersionData], item.type) : '无';
        }
        return (
          <Field label={item.label}>
            {content}
          </Field>
        );
      });
    }
    return [<ReleaseDate />,
      <LatestUpdateUser />,
      <LatestUpdateDate />,
      <CreateUser />,
      <CreateDate />,
      <Description maxWidth={maxWidth} />];
  });
  return (
    <div ref={ref} className={styles.fields}>
      {fields}
    </div>
  );
}
PublishVersionDetail.defaultProps = {
  customFields: undefined,
};
export default observer(PublishVersionDetail);
