import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { TextField } from 'choerodon-ui/pro/lib';
import TextEditToggle from '@/components/TextEditTogglePro';
import moment, { Moment } from 'moment';
import SelectAppService from '@/components/select/select-app-service';
import Field from '../field';
import { useReleaseDetailContext } from '../../../stores';

interface Props {

}
const AppService: React.FC<Props> = () => {
  const { disabled, store } = useReleaseDetailContext();
  const { serviceCode } = store.getCurrentData;
  return (
    <Field label="应用服务">
      <TextEditToggle
        onSubmit={(value: string | null) => {
          store.update({ serviceCode: value, tagName: null });
        }}
        disabled={disabled}
        initValue={serviceCode || undefined}
        submitTrigger={['blur', 'change']}
        editor={() => (
          <SelectAppService />
        )}
      >
        {serviceCode || '无'}
      </TextEditToggle>
    </Field>

  );
};

export default observer(AppService);
