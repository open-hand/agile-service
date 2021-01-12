import React from 'react';
import { observer } from 'mobx-react-lite';
import { DateTimePicker } from 'choerodon-ui/pro';
import moment from 'moment';
import TextEditToggle from '@/components/TextEditTogglePro';
import { useDetailContext } from '@/components/IssueDetail/context';
import Field from '../field';
import fieldStyles from '../custom-fields/index.less';

interface Props {
  disabled: boolean
}
const StartTime: React.FC<Props> = ({ disabled }) => {
  const { store } = useDetailContext();
  const { issue } = store;
  const { estimatedStartTime, estimatedEndTime } = issue;
  return (
    <Field label="预计结束时间">
      <TextEditToggle
        className={fieldStyles.select}
        disabled={disabled}
        onSubmit={(value: number) => {
          store.update('estimatedEndTime', value);
        }}
        initValue={estimatedEndTime ? moment(estimatedEndTime) : undefined}
        alwaysRender={false}
        submitTrigger={['blur']}
        editor={() => (
          <DateTimePicker min={estimatedStartTime && moment(estimatedStartTime).add(1, 's')} />
        )}
      >
        {
          estimatedEndTime || '无'
        }
      </TextEditToggle>
    </Field>
  );
};

export default observer(StartTime);
