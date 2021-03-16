import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import moment from 'moment';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const CreateDate: React.FC<Props> = () => {
  const { store } = useReleaseDetailContext();
  const { creationDate: originData } = store.getCurrentData;
  const creationDate = useMemo(() => (originData ? moment(originData, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD') : undefined), [originData]);

  return (
    <Field label="创建时间">
      {creationDate || '无'}
    </Field>

  );
};

export default observer(CreateDate);
