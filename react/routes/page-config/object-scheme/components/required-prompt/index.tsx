import React, { useState, useEffect, useCallback } from 'react';
import { IModalProps } from '@/common/types';
import { Button, CheckBox } from 'choerodon-ui/pro';

import { InjectedIntl } from 'react-intl';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import promptStyles from './index.less';

interface Props {
  modal?: IModalProps,
  formatMessage: InjectedIntl['formatMessage'],
  onContinue: (secondEntry?: boolean) => void,
  promptText: string,
}
const RequiredPrompt: React.FC<Props> = ({
  modal, formatMessage, onContinue, promptText,
}) => {
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
