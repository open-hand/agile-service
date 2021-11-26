import React, { useState, useEffect, useCallback } from 'react';
import { Button, CheckBox } from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { IModalProps } from '@/common/types';
import promptStyles from './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Props {
  modal?: IModalProps,
  onContinue: (secondEntry?: boolean) => void,
  promptText: string,
}
const RequiredPrompt: React.FC<Props> = ({
  modal, onContinue, promptText,
}) => {
  const formatMessage = useFormatMessage('agile.page');
  const [isPrompt, setIsPrompt] = useState<string>('true');

  const handleConfirm = useCallback(async () => {
    localStorage.setItem('agile.page.field.setting.required.prompt', isPrompt);
    onContinue(true);
    return true;
  }, [isPrompt, onContinue]);
  useEffect(() => {
    modal?.handleOk(handleConfirm);
  }, [handleConfirm, modal]);
  return (
    <>
      <div className={promptStyles.content}>
        <span className={promptStyles.text}>
          {promptText}
        </span>
        <CheckBox
          style={{ marginTop: 16 }}
          value="false"
          onChange={setIsPrompt}
          defaultChecked={false}
        >
          不再提示
        </CheckBox>
      </div>
    </>
  );
};
export default observer(RequiredPrompt);
