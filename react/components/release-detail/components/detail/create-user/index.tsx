import React from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import UserTag from '@/components/tag/user-tag';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const BusinessValue: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { creator } = store.getCurrentData;
  return (
    <Field label="创建人">
      {creator ? <UserTag data={creator} /> : '无'}
    </Field>

  );
};

export default observer(BusinessValue);
