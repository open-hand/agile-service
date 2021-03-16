import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { DatePicker } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import moment, { Moment } from 'moment';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const StartDate: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { startDate: originData, expectReleaseDate } = store.getCurrentData;
  const startDate = useMemo(() => (originData ? moment(originData, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD') : undefined), [originData]);
  return (
    <Field label="开始时间">
      <TextEditToggle
        onSubmit={(value: Moment | null) => {
          store.update('startDate', value?.format('YYYY-MM-DD HH:mm:ss'));
        }}
        disabled={disabled}
        initValue={startDate ? moment(startDate, 'YYYY-MM-DD') : undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (
          <DatePicker style={{ width: '100%' }} />
        )}
      >
        {startDate || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(StartDate);
