import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Field from '../field';

const ReleaseDate: React.FC = () => {
  const { store } = usePublishVersionContext();
  const { actualPublishDate: originData } = store.getCurrentData;
  const actualPublishDate = useMemo(() => (originData ? moment(originData, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD HH:mm:ss') : undefined), [originData]);

  return (
    <Field label="发布时间">
      {actualPublishDate || '无'}
    </Field>
  );
};

export default observer(ReleaseDate);
