interface IFieldModeInput {
    fields?: any[]
    systemFields?: any[]
    customFields?: any[]
}
type IFieldModeInputModeType = 'edit' | 'create'
interface IFieldModeConfig {
    mode: IFieldModeInputModeType
}
/**
 * 根据模型获取对应的字段
 * @param fieldInput
 * @param config
 */
function getFiledByMode(fieldInput: IFieldModeInput, config: IFieldModeConfig) {

}

export default getFiledByMode;
