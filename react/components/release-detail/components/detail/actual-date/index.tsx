import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const ActualDate: React.FC<Props> = () => {
  const { store } = useReleaseDetailContext();
  const { releaseDate: originData } = store.getCurrentData;
  const releaseDate = useMemo(() => (originData ? moment(originData, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD') : undefined), [originData]);

  return (
    <Field label="实际发布时间">
      {releaseDate || '无'}
    </Field>

  );
};

export default observer(ActualDate);
