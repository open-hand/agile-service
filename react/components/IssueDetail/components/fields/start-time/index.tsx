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
    <Field label="预计开始时间">
      <TextEditToggle
        className={fieldStyles.select}
        disabled={disabled}
        onSubmit={(value: number) => {
          store.update('estimatedStartTime', value);
        }}
        initValue={estimatedStartTime ? moment(estimatedStartTime) : undefined}
        alwaysRender={false}
        submitTrigger={['blur']}
        editor={() => (
          <DateTimePicker max={estimatedEndTime && moment(estimatedEndTime).subtract(1, 's')} />
        )}
      >
        {
          estimatedStartTime || '无'
        }
      </TextEditToggle>
    </Field>
  );
};

export default observer(StartTime);
