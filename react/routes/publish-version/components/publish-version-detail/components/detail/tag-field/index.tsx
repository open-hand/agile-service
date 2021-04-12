import React from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectGitTags from '@/components/select/select-git-tags';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const TagField: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { tagId, serviceCode } = store.getCurrentData;
  return (
    <Field label="Tag">
      <TextEditToggle
        onSubmit={(value: string | null) => {
          store.update('tagId', value);
        }}
        disabled={disabled}
        initValue={tagId || undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (<SelectGitTags applicationId={serviceCode} help={!serviceCode ? '请先选择应用服务' : undefined} />
        )}
      >
        {null || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(TagField);
