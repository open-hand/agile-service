import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectGitTags from '@/components/select/select-git-tags';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const TagField: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { tagName, serviceCode } = store.getCurrentData;
  const applicationId = useMemo(() => store.getAppServiceList.find((i) => i.code === serviceCode)?.id, [serviceCode, store.getAppServiceList]);
  return (
    <Field label="Tag">
      <TextEditToggle
        onSubmit={(value: string | null) => {
          store.update('tagName', value);
        }}
        disabled={disabled}
        initValue={tagName || undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (<SelectGitTags key={`select-git-tag-${applicationId}`} applicationId={applicationId} help={!applicationId ? '请先选择应用服务' : undefined} />
        )}
      >
        {tagName || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(TagField);
