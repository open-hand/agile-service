import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Field from '../field';

interface Props {

}
const LatestUpdateDate: React.FC<Props> = () => {
  const { store } = usePublishVersionContext();
  const { lastUpdateDate: originData } = store.getCurrentData;
  const lastUpdateDate = useMemo(() => (originData ? moment(originData, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD HH:mm:ss') : undefined), [originData]);

  return (
    <Field label="最近更新时间">
      {lastUpdateDate || '无'}
    </Field>

  );
};

export default observer(LatestUpdateDate);
