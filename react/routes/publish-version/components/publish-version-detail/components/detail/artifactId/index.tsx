import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { TextField } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import moment, { Moment } from 'moment';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const ArtifactId: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { artifactId } = store.getCurrentData;
  return (
    <Field label="artifactId">
      <TextEditToggle
        onSubmit={(value: string | null) => {
          store.update('artifactId', value);
        }}
        disabled={disabled}
        initValue={artifactId || undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (
          <TextField />
        )}
      >
        {artifactId || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(ArtifactId);
