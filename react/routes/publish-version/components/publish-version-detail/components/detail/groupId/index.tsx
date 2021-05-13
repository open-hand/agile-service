import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { TextField } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import moment, { Moment } from 'moment';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const GroupId: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { groupId } = store.getCurrentData;
  return (
    <Field label="groupId">
      <TextEditToggle
        onSubmit={(value: string | null) => {
          store.update('groupId', value);
        }}
        disabled={disabled}
        initValue={groupId || undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (
          <TextField />
        )}
      >
        {groupId || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(GroupId);
