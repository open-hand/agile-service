import { IField } from '@/common/types';
import { DatePickerProps } from 'choerodon-ui/pro/lib/date-picker/DatePicker';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface IChosenFieldField extends Partial<IField> {
    id?: string,
    code: string,
    name: string,
    defaultShow?: boolean,
    fieldType?: string,
    noDisplay?: boolean,
    immutableCheck?: boolean, // 存在此属性 则不可删除 禁止选择
    otherComponentProps?: Partial<SelectProps> | Partial<DatePickerProps> // 组件的属性
    value?: any,

}
interface IUseChoseFieldProps {
    systemFields: IChosenFieldField[]
    customFields: IChosenFieldField[],
    chosenFields?: IChosenFieldField[],
}
export { IChosenFieldField, IUseChoseFieldProps };
