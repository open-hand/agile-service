import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const BusinessValue: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { actualReleaseDate } = store.getCurrentData;
  return (
    <Field label="实际发布时间">

      {actualReleaseDate || '无'}
    </Field>

  );
};

export default observer(BusinessValue);
