import React, {
  createContext, useContext,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import useHeaderFullScreenButton from '@/hooks/useHeaderButtons/useHeaderFullScreenButton';

interface WorkBenchWorkingHoursContext {
    fullButtonProps?: ReturnType<typeof useHeaderFullScreenButton>[1]
}
const Context = createContext({ fullButtonProps: {} as any } as WorkBenchWorkingHoursContext);
export function useWorkBenchWorkingHoursContext() {
  return useContext(Context);
}
const WorkBenchWorkingHoursProvider = inject('AppState')(injectIntl(
  (props: any) => {
    const {
      children,
    } = props;
    const [, fullButtonProps] = useHeaderFullScreenButton({ appendCustomClassName: 'c7n-agile-working-hours-full-screen' });
    return (
      <Context.Provider value={{ fullButtonProps }}>
        {children}
      </Context.Provider>
    );
  },
));

export default WorkBenchWorkingHoursProvider;
