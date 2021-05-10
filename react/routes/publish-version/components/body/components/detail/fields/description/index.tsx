import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker, TextArea } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import moment, { Moment } from 'moment';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import Field from '../field';

interface Props {

}
const BusinessValue: React.FC<Props> = () => {
  const { store } = usePublishVersionContext();
  const { description } = store.getCurrentData;

  return (

    <Field label="描述">
      <TextEditToggle
        onSubmit={(value: Moment | null) => {
          // store.update('expectReleaseDate', value?.format('YYYY-MM-DD HH:mm:ss'));
        }}
        // disabled={disabled}
        initValue={description}
        submitTrigger={['blur']}
        editor={() => (
          <TextArea style={{ width: '100%' }} />
        )}
      >
        {description || '无'}
      </TextEditToggle>
    </Field>
  );
};

export default observer(BusinessValue);
