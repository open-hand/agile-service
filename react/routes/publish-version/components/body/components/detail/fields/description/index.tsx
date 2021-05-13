import React from 'react';
import { observer } from 'mobx-react-lite';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Field from '../field';

interface Props {

}
const BusinessValue: React.FC<Props> = () => {
  const { store } = usePublishVersionContext();
  const { description } = store.getCurrentData;
  return (
    <Field label="描述">
      {description || '无'}
    </Field>
  );
};

export default observer(BusinessValue);
