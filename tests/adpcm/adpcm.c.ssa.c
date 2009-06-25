void adpcm_coder(short *indata@adpcm_coder , char *outdata@adpcm_coder ,
                 int nsample@adpcm_coder ,
                 struct adpcm_state *state@adpcm_coder )
{ short *inp@adpcm_coder ;
  signed char *outp@adpcm_coder ;
  int val@adpcm_coder ;
  int sign@adpcm_coder ;
  int delta@adpcm_coder ;
  int diff@adpcm_coder ;
  int step@adpcm_coder ;
  int valpred@adpcm_coder ;
  int vpdiff@adpcm_coder ;
  int index@adpcm_coder ;
  int outputbuffer@adpcm_coder ;
  int bufferstep@adpcm_coder ;
  int len@adpcm_coder ;
  short *tmp@adpcm_coder ;
  signed char *tmp___0@adpcm_coder ;
  signed char *tmp___1@adpcm_coder ;
  unsigned int __cil_tmp23@adpcm_coder ;
  char *__cil_tmp24@adpcm_coder ;
  unsigned int __cil_tmp25@adpcm_coder ;
  char *__cil_tmp26@adpcm_coder ;
  unsigned int __cil_tmp27@adpcm_coder ;
  char *__cil_tmp28@adpcm_coder ;
  unsigned int __cil_tmp29@adpcm_coder ;
  char *__cil_tmp30@adpcm_coder ;
  unsigned int __cil_tmp31@adpcm_coder ;
  char *__cil_tmp32@adpcm_coder ;
  unsigned int __cil_tmp33@adpcm_coder ;
  char *__cil_tmp34@adpcm_coder ;
  unsigned int __cil_tmp35@adpcm_coder ;
  char *__cil_tmp36@adpcm_coder ;
  unsigned int __cil_tmp37@adpcm_coder ;
  char *__cil_tmp38@adpcm_coder ;
  unsigned int __cil_tmp39@adpcm_coder ;
  char *__cil_tmp40@adpcm_coder ;
  unsigned int __cil_tmp41@adpcm_coder ;
  char *__cil_tmp42@adpcm_coder ;
  unsigned int __cil_tmp43@adpcm_coder ;
  char *__cil_tmp44@adpcm_coder ;
  unsigned int __cil_tmp45@adpcm_coder ;
  char *__cil_tmp46@adpcm_coder ;
  unsigned int __cil_tmp47@adpcm_coder ;
  char *__cil_tmp48@adpcm_coder ;
  unsigned int __cil_tmp49@adpcm_coder ;
  char *__cil_tmp50@adpcm_coder ;
  unsigned int __cil_tmp51@adpcm_coder ;
  char *__cil_tmp52@adpcm_coder ;
  unsigned int __cil_tmp53@adpcm_coder ;
  char *__cil_tmp54@adpcm_coder ;
  unsigned int __cil_tmp55@adpcm_coder ;
  char *__cil_tmp56@adpcm_coder ;
  unsigned int __cil_tmp57@adpcm_coder ;
  char *__cil_tmp58@adpcm_coder ;
  unsigned int __cil_tmp59@adpcm_coder ;
  char *__cil_tmp60@adpcm_coder ;
  unsigned int __cil_tmp61@adpcm_coder ;
  char *__cil_tmp62@adpcm_coder ;
  unsigned int __cil_tmp63@adpcm_coder ;
  char *__cil_tmp64@adpcm_coder ;
  unsigned int __cil_tmp65@adpcm_coder ;
  char *__cil_tmp66@adpcm_coder ;
  unsigned int __cil_tmp67@adpcm_coder ;
  char *__cil_tmp68@adpcm_coder ;
  unsigned int __cil_tmp69@adpcm_coder ;
  char *__cil_tmp70@adpcm_coder ;
  unsigned int __cil_tmp71@adpcm_coder ;
  char *__cil_tmp72@adpcm_coder ;
  unsigned int __cil_tmp73@adpcm_coder ;
  char *__cil_tmp74@adpcm_coder ;
  unsigned int __cil_tmp75@adpcm_coder ;
  char *__cil_tmp76@adpcm_coder ;
  unsigned int __cil_tmp77@adpcm_coder ;
  char *__cil_tmp78@adpcm_coder ;
  unsigned int __cil_tmp79@adpcm_coder ;
  char *__cil_tmp80@adpcm_coder ;
  unsigned int __cil_tmp81@adpcm_coder ;
  char *__cil_tmp82@adpcm_coder ;
  unsigned int __cil_tmp83@adpcm_coder ;
  char *__cil_tmp84@adpcm_coder ;
  unsigned int __cil_tmp85@adpcm_coder ;
  char *__cil_tmp86@adpcm_coder ;
  unsigned int __cil_tmp87@adpcm_coder ;
  char *__cil_tmp88@adpcm_coder ;
  unsigned int __cil_tmp89@adpcm_coder ;
  char *__cil_tmp90@adpcm_coder ;
  unsigned int __cil_tmp91@adpcm_coder ;
  char *__cil_tmp92@adpcm_coder ;
  unsigned int __cil_tmp93@adpcm_coder ;
  char *__cil_tmp94@adpcm_coder ;
  unsigned int __cil_tmp95@adpcm_coder ;
  char *__cil_tmp96@adpcm_coder ;
  unsigned int __cil_tmp97@adpcm_coder ;
  char *__cil_tmp98@adpcm_coder ;
  unsigned int __cil_tmp99@adpcm_coder ;
  char *__cil_tmp100@adpcm_coder ;
  unsigned int __cil_tmp101@adpcm_coder ;
  char *__cil_tmp102@adpcm_coder ;
  unsigned int __cil_tmp103@adpcm_coder ;
  char *__cil_tmp104@adpcm_coder ;
  unsigned int __cil_tmp105@adpcm_coder ;
  char *__cil_tmp106@adpcm_coder ;
  unsigned int __cil_tmp107@adpcm_coder ;
  char *__cil_tmp108@adpcm_coder ;
  unsigned int __cil_tmp109@adpcm_coder ;
  char *__cil_tmp110@adpcm_coder ;
  unsigned int __cil_tmp111@adpcm_coder ;
  char *__cil_tmp112@adpcm_coder ;
  unsigned int __cil_tmp113@adpcm_coder ;
  char *__cil_tmp114@adpcm_coder ;
  unsigned int __cil_tmp115@adpcm_coder ;
  char *__cil_tmp116@adpcm_coder ;
  unsigned int __cil_tmp117@adpcm_coder ;
  char *__cil_tmp118@adpcm_coder ;
  unsigned int __cil_tmp119@adpcm_coder ;
  char *__cil_tmp120@adpcm_coder ;
  unsigned int __cil_tmp121@adpcm_coder ;
  char *__cil_tmp122@adpcm_coder ;
  unsigned int __cil_tmp123@adpcm_coder ;
  char *__cil_tmp124@adpcm_coder ;
  unsigned int __cil_tmp125@adpcm_coder ;
  char *__cil_tmp126@adpcm_coder ;
  unsigned int __cil_tmp127@adpcm_coder ;
  char *__cil_tmp128@adpcm_coder ;
  unsigned int __cil_tmp129@adpcm_coder ;
  char *__cil_tmp130@adpcm_coder ;
  unsigned int __cil_tmp131@adpcm_coder ;
  char *__cil_tmp132@adpcm_coder ;
  unsigned int __cil_tmp133@adpcm_coder ;
  char *__cil_tmp134@adpcm_coder ;
  unsigned int __cil_tmp135@adpcm_coder ;
  char *__cil_tmp136@adpcm_coder ;
  unsigned int __cil_tmp137@adpcm_coder ;
  char *__cil_tmp138@adpcm_coder ;
  unsigned int __cil_tmp139@adpcm_coder ;
  char *__cil_tmp140@adpcm_coder ;
  unsigned int __cil_tmp141@adpcm_coder ;
  char *__cil_tmp142@adpcm_coder ;
  unsigned int __cil_tmp143@adpcm_coder ;
  char *__cil_tmp144@adpcm_coder ;
  unsigned int __cil_tmp145@adpcm_coder ;
  char *__cil_tmp146@adpcm_coder ;
  unsigned int __cil_tmp147@adpcm_coder ;
  char *__cil_tmp148@adpcm_coder ;
  unsigned int __cil_tmp149@adpcm_coder ;
  char *__cil_tmp150@adpcm_coder ;
  unsigned int __cil_tmp151@adpcm_coder ;
  char *__cil_tmp152@adpcm_coder ;
  unsigned int __cil_tmp153@adpcm_coder ;
  char *__cil_tmp154@adpcm_coder ;
  unsigned int __cil_tmp155@adpcm_coder ;
  char *__cil_tmp156@adpcm_coder ;
  unsigned int __cil_tmp157@adpcm_coder ;
  char *__cil_tmp158@adpcm_coder ;
  unsigned int __cil_tmp159@adpcm_coder ;
  char *__cil_tmp160@adpcm_coder ;
  unsigned int __cil_tmp161@adpcm_coder ;
  char *__cil_tmp162@adpcm_coder ;
  unsigned int __cil_tmp163@adpcm_coder ;
  char *__cil_tmp164@adpcm_coder ;
  unsigned int __cil_tmp165@adpcm_coder ;
  char *__cil_tmp166@adpcm_coder ;
  unsigned int __cil_tmp167@adpcm_coder ;
  char *__cil_tmp168@adpcm_coder ;
  unsigned int __cil_tmp169@adpcm_coder ;
  char *__cil_tmp170@adpcm_coder ;
  unsigned int __cil_tmp171@adpcm_coder ;
  char *__cil_tmp172@adpcm_coder ;
  unsigned int __cil_tmp173@adpcm_coder ;
  char *__cil_tmp174@adpcm_coder ;
  unsigned int __cil_tmp175@adpcm_coder ;
  char *__cil_tmp176@adpcm_coder ;
  unsigned int __cil_tmp177@adpcm_coder ;
  char *__cil_tmp178@adpcm_coder ;
  unsigned int __cil_tmp179@adpcm_coder ;
  char *__cil_tmp180@adpcm_coder ;
  unsigned int __cil_tmp181@adpcm_coder ;
  char *__cil_tmp182@adpcm_coder ;
  unsigned int __cil_tmp183@adpcm_coder ;
  char *__cil_tmp184@adpcm_coder ;
  unsigned int __cil_tmp185@adpcm_coder ;
  char *__cil_tmp186@adpcm_coder ;
  unsigned int __cil_tmp187@adpcm_coder ;
  char *__cil_tmp188@adpcm_coder ;
  unsigned int __cil_tmp189@adpcm_coder ;
  char *__cil_tmp190@adpcm_coder ;
  unsigned int __cil_tmp191@adpcm_coder ;
  char *__cil_tmp192@adpcm_coder ;
  unsigned int __cil_tmp193@adpcm_coder ;
  char *__cil_tmp194@adpcm_coder ;
  unsigned int __cil_tmp195@adpcm_coder ;
  char *__cil_tmp196@adpcm_coder ;
  unsigned int __cil_tmp197@adpcm_coder ;
  char *__cil_tmp198@adpcm_coder ;
  unsigned int __cil_tmp199@adpcm_coder ;
  char *__cil_tmp200@adpcm_coder ;
  unsigned int __cil_tmp201@adpcm_coder ;
  char *__cil_tmp202@adpcm_coder ;
  unsigned int __cil_tmp203@adpcm_coder ;
  char *__cil_tmp204@adpcm_coder ;
  unsigned int __cil_tmp205@adpcm_coder ;
  char *__cil_tmp206@adpcm_coder ;
  unsigned int __cil_tmp207@adpcm_coder ;
  char *__cil_tmp208@adpcm_coder ;
  unsigned int __cil_tmp209@adpcm_coder ;
  char *__cil_tmp210@adpcm_coder ;
  unsigned int __cil_tmp211@adpcm_coder ;
  char *__cil_tmp212@adpcm_coder ;
  unsigned int __cil_tmp213@adpcm_coder ;
  char *__cil_tmp214@adpcm_coder ;
  unsigned int __cil_tmp215@adpcm_coder ;
  char *__cil_tmp216@adpcm_coder ;
  unsigned int __cil_tmp217@adpcm_coder ;
  char *__cil_tmp218@adpcm_coder ;
  unsigned int __cil_tmp219@adpcm_coder ;
  char *__cil_tmp220@adpcm_coder ;
  unsigned int __cil_tmp221@adpcm_coder ;
  char *__cil_tmp222@adpcm_coder ;
  unsigned int __cil_tmp223@adpcm_coder ;
  char *__cil_tmp224@adpcm_coder ;
  unsigned int __cil_tmp225@adpcm_coder ;
  char *__cil_tmp226@adpcm_coder ;
  unsigned int __cil_tmp227@adpcm_coder ;
  char *__cil_tmp228@adpcm_coder ;
  unsigned int __cil_tmp229@adpcm_coder ;
  char *__cil_tmp230@adpcm_coder ;
  unsigned int __cil_tmp231@adpcm_coder ;
  char *__cil_tmp232@adpcm_coder ;
  short __cil_tmp233@adpcm_coder ;
  char *__cil_tmp234@adpcm_coder ;
  char *__cil_tmp235@adpcm_coder ;
  char __cil_tmp236@adpcm_coder ;
  unsigned int __cil_tmp237@adpcm_coder ;
  char *__cil_tmp238@adpcm_coder ;
  short __cil_tmp239@adpcm_coder ;
  unsigned int __cil_tmp240@adpcm_coder ;
  char *__cil_tmp241@adpcm_coder ;
  int __cil_tmp242@adpcm_coder ;
  unsigned int __cil_tmp243@adpcm_coder ;
  char *__cil_tmp244@adpcm_coder ;
  int __cil_tmp245@adpcm_coder ;
  int __cil_tmp246@adpcm_coder ;
  int __cil_tmp247@adpcm_coder ;
  char *__cil_tmp248@adpcm_coder ;
  char *__cil_tmp249@adpcm_coder ;
  int (*indexTable#heapify@adpcm_coder)[16] ;
  int (*stepsizeTable#heapify@adpcm_coder)[89] ;
  int __cil_tmp252@adpcm_coder[16] ;
  int __cil_tmp253@adpcm_coder ;
  int __cil_tmp254@adpcm_coder[16] ;
  int __cil_tmp255@adpcm_coder ;
  int __cil_tmp256@adpcm_coder[16] ;
  int __cil_tmp257@adpcm_coder ;
  int __cil_tmp258@adpcm_coder[16] ;
  int __cil_tmp259@adpcm_coder ;
  int __cil_tmp260@adpcm_coder[16] ;
  int __cil_tmp261@adpcm_coder ;
  int __cil_tmp262@adpcm_coder[16] ;
  int __cil_tmp263@adpcm_coder ;
  int __cil_tmp264@adpcm_coder[16] ;
  int __cil_tmp265@adpcm_coder ;
  int __cil_tmp266@adpcm_coder[16] ;
  int __cil_tmp267@adpcm_coder ;
  int __cil_tmp268@adpcm_coder[16] ;
  int __cil_tmp269@adpcm_coder ;
  int __cil_tmp270@adpcm_coder[16] ;
  int __cil_tmp271@adpcm_coder ;
  int __cil_tmp272@adpcm_coder[16] ;
  int __cil_tmp273@adpcm_coder ;
  int __cil_tmp274@adpcm_coder[16] ;
  int __cil_tmp275@adpcm_coder ;
  int __cil_tmp276@adpcm_coder[16] ;
  int __cil_tmp277@adpcm_coder ;
  int __cil_tmp278@adpcm_coder[16] ;
  int __cil_tmp279@adpcm_coder ;
  int __cil_tmp280@adpcm_coder[16] ;
  int __cil_tmp281@adpcm_coder ;
  int __cil_tmp282@adpcm_coder[16] ;
  int __cil_tmp283@adpcm_coder ;
  int __cil_tmp284@adpcm_coder[89] ;
  int __cil_tmp285@adpcm_coder ;
  int __cil_tmp286@adpcm_coder[89] ;
  int __cil_tmp287@adpcm_coder ;
  int __cil_tmp288@adpcm_coder[89] ;
  int __cil_tmp289@adpcm_coder ;
  int __cil_tmp290@adpcm_coder[89] ;
  int __cil_tmp291@adpcm_coder ;
  int __cil_tmp292@adpcm_coder[89] ;
  int __cil_tmp293@adpcm_coder ;
  int __cil_tmp294@adpcm_coder[89] ;
  int __cil_tmp295@adpcm_coder ;
  int __cil_tmp296@adpcm_coder[89] ;
  int __cil_tmp297@adpcm_coder ;
  int __cil_tmp298@adpcm_coder[89] ;
  int __cil_tmp299@adpcm_coder ;
  int __cil_tmp300@adpcm_coder[89] ;
  int __cil_tmp301@adpcm_coder ;
  int __cil_tmp302@adpcm_coder[89] ;
  int __cil_tmp303@adpcm_coder ;
  int __cil_tmp304@adpcm_coder[89] ;
  int __cil_tmp305@adpcm_coder ;
  int __cil_tmp306@adpcm_coder[89] ;
  int __cil_tmp307@adpcm_coder ;
  int __cil_tmp308@adpcm_coder[89] ;
  int __cil_tmp309@adpcm_coder ;
  int __cil_tmp310@adpcm_coder[89] ;
  int __cil_tmp311@adpcm_coder ;
  int __cil_tmp312@adpcm_coder[89] ;
  int __cil_tmp313@adpcm_coder ;
  int __cil_tmp314@adpcm_coder[89] ;
  int __cil_tmp315@adpcm_coder ;
  int __cil_tmp316@adpcm_coder[89] ;
  int __cil_tmp317@adpcm_coder ;
  int __cil_tmp318@adpcm_coder[89] ;
  int __cil_tmp319@adpcm_coder ;
  int __cil_tmp320@adpcm_coder[89] ;
  int __cil_tmp321@adpcm_coder ;
  int __cil_tmp322@adpcm_coder[89] ;
  int __cil_tmp323@adpcm_coder ;
  int __cil_tmp324@adpcm_coder[89] ;
  int __cil_tmp325@adpcm_coder ;
  int __cil_tmp326@adpcm_coder[89] ;
  int __cil_tmp327@adpcm_coder ;
  int __cil_tmp328@adpcm_coder[89] ;
  int __cil_tmp329@adpcm_coder ;
  int __cil_tmp330@adpcm_coder[89] ;
  int __cil_tmp331@adpcm_coder ;
  int __cil_tmp332@adpcm_coder[89] ;
  int __cil_tmp333@adpcm_coder ;
  int __cil_tmp334@adpcm_coder[89] ;
  int __cil_tmp335@adpcm_coder ;
  int __cil_tmp336@adpcm_coder[89] ;
  int __cil_tmp337@adpcm_coder ;
  int __cil_tmp338@adpcm_coder[89] ;
  int __cil_tmp339@adpcm_coder ;
  int __cil_tmp340@adpcm_coder[89] ;
  int __cil_tmp341@adpcm_coder ;
  int __cil_tmp342@adpcm_coder[89] ;
  int __cil_tmp343@adpcm_coder ;
  int __cil_tmp344@adpcm_coder[89] ;
  int __cil_tmp345@adpcm_coder ;
  int __cil_tmp346@adpcm_coder[89] ;
  int __cil_tmp347@adpcm_coder ;
  int __cil_tmp348@adpcm_coder[89] ;
  int __cil_tmp349@adpcm_coder ;
  int __cil_tmp350@adpcm_coder[89] ;
  int __cil_tmp351@adpcm_coder ;
  int __cil_tmp352@adpcm_coder[89] ;
  int __cil_tmp353@adpcm_coder ;
  int __cil_tmp354@adpcm_coder[89] ;
  int __cil_tmp355@adpcm_coder ;
  int __cil_tmp356@adpcm_coder[89] ;
  int __cil_tmp357@adpcm_coder ;
  int __cil_tmp358@adpcm_coder[89] ;
  int __cil_tmp359@adpcm_coder ;
  int __cil_tmp360@adpcm_coder[89] ;
  int __cil_tmp361@adpcm_coder ;
  int __cil_tmp362@adpcm_coder[89] ;
  int __cil_tmp363@adpcm_coder ;
  int __cil_tmp364@adpcm_coder[89] ;
  int __cil_tmp365@adpcm_coder ;
  int __cil_tmp366@adpcm_coder[89] ;
  int __cil_tmp367@adpcm_coder ;
  int __cil_tmp368@adpcm_coder[89] ;
  int __cil_tmp369@adpcm_coder ;
  int __cil_tmp370@adpcm_coder[89] ;
  int __cil_tmp371@adpcm_coder ;
  int __cil_tmp372@adpcm_coder[89] ;
  int __cil_tmp373@adpcm_coder ;
  int __cil_tmp374@adpcm_coder[89] ;
  int __cil_tmp375@adpcm_coder ;
  int __cil_tmp376@adpcm_coder[89] ;
  int __cil_tmp377@adpcm_coder ;
  int __cil_tmp378@adpcm_coder[89] ;
  int __cil_tmp379@adpcm_coder ;
  int __cil_tmp380@adpcm_coder[89] ;
  int __cil_tmp381@adpcm_coder ;
  int __cil_tmp382@adpcm_coder[89] ;
  int __cil_tmp383@adpcm_coder ;
  int __cil_tmp384@adpcm_coder[89] ;
  int __cil_tmp385@adpcm_coder ;
  int __cil_tmp386@adpcm_coder[89] ;
  int __cil_tmp387@adpcm_coder ;
  int __cil_tmp388@adpcm_coder[89] ;
  int __cil_tmp389@adpcm_coder ;
  int __cil_tmp390@adpcm_coder[89] ;
  int __cil_tmp391@adpcm_coder ;
  int __cil_tmp392@adpcm_coder[89] ;
  int __cil_tmp393@adpcm_coder ;
  int __cil_tmp394@adpcm_coder[89] ;
  int __cil_tmp395@adpcm_coder ;
  int __cil_tmp396@adpcm_coder[89] ;
  int __cil_tmp397@adpcm_coder ;
  int __cil_tmp398@adpcm_coder[89] ;
  int __cil_tmp399@adpcm_coder ;
  int __cil_tmp400@adpcm_coder[89] ;
  int __cil_tmp401@adpcm_coder ;
  int __cil_tmp402@adpcm_coder[89] ;
  int __cil_tmp403@adpcm_coder ;
  int __cil_tmp404@adpcm_coder[89] ;
  int __cil_tmp405@adpcm_coder ;
  int __cil_tmp406@adpcm_coder[89] ;
  int __cil_tmp407@adpcm_coder ;
  int __cil_tmp408@adpcm_coder[89] ;
  int __cil_tmp409@adpcm_coder ;
  int __cil_tmp410@adpcm_coder[89] ;
  int __cil_tmp411@adpcm_coder ;
  int __cil_tmp412@adpcm_coder[89] ;
  int __cil_tmp413@adpcm_coder ;
  int __cil_tmp414@adpcm_coder[89] ;
  int __cil_tmp415@adpcm_coder ;
  int __cil_tmp416@adpcm_coder[89] ;
  int __cil_tmp417@adpcm_coder ;
  int __cil_tmp418@adpcm_coder[89] ;
  int __cil_tmp419@adpcm_coder ;
  int __cil_tmp420@adpcm_coder[89] ;
  int __cil_tmp421@adpcm_coder ;
  int __cil_tmp422@adpcm_coder[89] ;
  int __cil_tmp423@adpcm_coder ;
  int __cil_tmp424@adpcm_coder[89] ;
  int __cil_tmp425@adpcm_coder ;
  int __cil_tmp426@adpcm_coder[89] ;
  int __cil_tmp427@adpcm_coder ;
  int __cil_tmp428@adpcm_coder[89] ;
  int __cil_tmp429@adpcm_coder ;
  int __cil_tmp430@adpcm_coder[89] ;
  int __cil_tmp431@adpcm_coder ;
  int __cil_tmp432@adpcm_coder[89] ;
  int __cil_tmp433@adpcm_coder ;
  int __cil_tmp434@adpcm_coder[89] ;
  int __cil_tmp435@adpcm_coder ;
  int __cil_tmp436@adpcm_coder[89] ;
  int __cil_tmp437@adpcm_coder ;
  int __cil_tmp438@adpcm_coder[89] ;
  int __cil_tmp439@adpcm_coder ;
  int __cil_tmp440@adpcm_coder[89] ;
  int __cil_tmp441@adpcm_coder ;
  int __cil_tmp442@adpcm_coder[89] ;
  int __cil_tmp443@adpcm_coder ;
  int __cil_tmp444@adpcm_coder[89] ;
  int __cil_tmp445@adpcm_coder ;
  int __cil_tmp446@adpcm_coder[89] ;
  int __cil_tmp447@adpcm_coder ;
  int __cil_tmp448@adpcm_coder[89] ;
  int __cil_tmp449@adpcm_coder ;
  int __cil_tmp450@adpcm_coder[89] ;
  int __cil_tmp451@adpcm_coder ;
  int __cil_tmp452@adpcm_coder[89] ;
  int __cil_tmp453@adpcm_coder ;
  int __cil_tmp454@adpcm_coder[89] ;
  int __cil_tmp455@adpcm_coder ;
  int __cil_tmp456@adpcm_coder[89] ;
  int __cil_tmp457@adpcm_coder ;
  int __cil_tmp458@adpcm_coder[89] ;
  int __cil_tmp459@adpcm_coder ;
  int __cil_tmp460@adpcm_coder[89] ;
  int __cil_tmp461@adpcm_coder ;
  int __cil_tmp462@adpcm_coder[89] ;
  int __cil_tmp463@adpcm_coder[16] ;
  int __cil_tmp464@adpcm_coder[89] ;
  signed char __cil_tmp465@adpcm_coder ;
  signed char __cil_tmp466@adpcm_coder ;
  short __cil_tmp467@adpcm_coder ;
  char __cil_tmp468@adpcm_coder ;
  signed char __cil_tmp465@adpcm_coder#phi_4 ;
  signed char __cil_tmp465@adpcm_coder#phi_38 ;
  int __cil_tmp247@adpcm_coder#phi_4 ;
  int __cil_tmp247@adpcm_coder#phi_38 ;
  int __cil_tmp246@adpcm_coder#phi_4 ;
  int __cil_tmp246@adpcm_coder#phi_38 ;
  signed char *tmp___0@adpcm_coder#phi_4 ;
  signed char *tmp___0@adpcm_coder#phi_38 ;
  int outputbuffer@adpcm_coder#phi_4 ;
  int outputbuffer@adpcm_coder#phi_38 ;
  int __cil_tmp245@adpcm_coder#phi_4 ;
  int __cil_tmp245@adpcm_coder#phi_38 ;
  char *__cil_tmp244@adpcm_coder#phi_4 ;
  char *__cil_tmp244@adpcm_coder#blk_34_1 ;
  int __cil_tmp464@adpcm_coder#phi_4[89] ;
  int __cil_tmp464@adpcm_coder#blk_34_1[89] ;
  unsigned int __cil_tmp243@adpcm_coder#phi_4 ;
  unsigned int __cil_tmp243@adpcm_coder#blk_34_1 ;
  int __cil_tmp242@adpcm_coder#phi_4 ;
  int __cil_tmp242@adpcm_coder#blk_29_1 ;
  char *__cil_tmp241@adpcm_coder#phi_4 ;
  char *__cil_tmp241@adpcm_coder#blk_29_1 ;
  int __cil_tmp463@adpcm_coder#phi_4[16] ;
  int __cil_tmp463@adpcm_coder#blk_29_1[16] ;
  unsigned int __cil_tmp240@adpcm_coder#phi_4 ;
  unsigned int __cil_tmp240@adpcm_coder#blk_29_1 ;
  int vpdiff@adpcm_coder#phi_4 ;
  int vpdiff@adpcm_coder#phi_22 ;
  int delta@adpcm_coder#phi_4 ;
  int delta@adpcm_coder#blk_29_1 ;
  int sign@adpcm_coder#phi_4 ;
  int sign@adpcm_coder#phi_11 ;
  int diff@adpcm_coder#phi_4 ;
  int diff@adpcm_coder#phi_19 ;
  int val@adpcm_coder#phi_4 ;
  int val@adpcm_coder#blk_7_1 ;
  short __cil_tmp239@adpcm_coder#phi_4 ;
  short __cil_tmp239@adpcm_coder#blk_7_1 ;
  short *tmp@adpcm_coder#phi_4 ;
  short *tmp@adpcm_coder#blk_7_1 ;
  int bufferstep@adpcm_coder#phi_4 ;
  int bufferstep@adpcm_coder#blk_38_1 ;
  int bufferstep@adpcm_coder#blk_1_1 ;
  int step@adpcm_coder#phi_4 ;
  int step@adpcm_coder#blk_34_1 ;
  int step@adpcm_coder#blk_1_1 ;
  int index@adpcm_coder#phi_4 ;
  int index@adpcm_coder#phi_34 ;
  int index@adpcm_coder#blk_1_1 ;
  int valpred@adpcm_coder#phi_4 ;
  int valpred@adpcm_coder#phi_29 ;
  int valpred@adpcm_coder#blk_1_1 ;
  short *inp@adpcm_coder#phi_4 ;
  short *inp@adpcm_coder#blk_7_1 ;
  short *inp@adpcm_coder#blk_1_1 ;
  signed char *outp@adpcm_coder#phi_4 ;
  signed char *outp@adpcm_coder#phi_38 ;
  signed char *outp@adpcm_coder#blk_1_1 ;
  int len@adpcm_coder#phi_4 ;
  int len@adpcm_coder#blk_38_1 ;
  int len@adpcm_coder#blk_1_1 ;
  int sign@adpcm_coder#blk_10_1 ;
  int sign@adpcm_coder#blk_9_1 ;
  int diff@adpcm_coder#phi_13 ;
  int diff@adpcm_coder#blk_7_1 ;
  int diff@adpcm_coder#blk_12_1 ;
  int vpdiff@adpcm_coder#phi_16 ;
  int vpdiff@adpcm_coder#blk_13_1 ;
  int vpdiff@adpcm_coder#blk_15_1 ;
  int delta@adpcm_coder#phi_16 ;
  int delta@adpcm_coder#blk_13_1 ;
  int delta@adpcm_coder#blk_15_1 ;
  int diff@adpcm_coder#phi_16 ;
  int diff@adpcm_coder#blk_15_1 ;
  int vpdiff@adpcm_coder#phi_19 ;
  int vpdiff@adpcm_coder#blk_18_1 ;
  int delta@adpcm_coder#phi_19 ;
  int delta@adpcm_coder#blk_18_1 ;
  int diff@adpcm_coder#blk_18_1 ;
  int vpdiff@adpcm_coder#blk_21_1 ;
  int delta@adpcm_coder#phi_22 ;
  int delta@adpcm_coder#blk_21_1 ;
  int valpred@adpcm_coder#phi_25 ;
  int valpred@adpcm_coder#blk_24_1 ;
  int valpred@adpcm_coder#blk_23_1 ;
  int valpred@adpcm_coder#blk_28_1 ;
  int valpred@adpcm_coder#blk_26_1 ;
  int index@adpcm_coder#phi_32 ;
  int index@adpcm_coder#blk_29_1 ;
  int index@adpcm_coder#blk_31_1 ;
  int index@adpcm_coder#blk_33_1 ;
  signed char __cil_tmp465@adpcm_coder#blk_37_2 ;
  int __cil_tmp247@adpcm_coder#blk_37_1 ;
  int __cil_tmp246@adpcm_coder#blk_37_1 ;
  signed char *tmp___0@adpcm_coder#blk_37_1 ;
  int outputbuffer@adpcm_coder#blk_36_1 ;
  int __cil_tmp245@adpcm_coder#blk_36_1 ;
  signed char *outp@adpcm_coder#blk_37_1 ;
  signed char __cil_tmp466@adpcm_coder#phi_42 ;
  signed char __cil_tmp466@adpcm_coder#blk_41_2 ;
  signed char *tmp___1@adpcm_coder#phi_42 ;
  signed char *tmp___1@adpcm_coder#blk_41_1 ;
  signed char *outp@adpcm_coder#phi_42 ;
  signed char *outp@adpcm_coder#blk_41_1 ;
  int (*indexTable#heapify@adpcm_coder#blk_0_1)[16] ;
  int (*stepsizeTable#heapify@adpcm_coder#blk_0_1)[89] ;
  unsigned int __cil_tmp23@adpcm_coder#blk_1_1 ;
  int __cil_tmp252@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp24@adpcm_coder#blk_1_1 ;
  int __cil_tmp253@adpcm_coder#blk_1_1 ;
  int __cil_tmp253@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp25@adpcm_coder#blk_1_1 ;
  int __cil_tmp254@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp26@adpcm_coder#blk_1_1 ;
  int __cil_tmp255@adpcm_coder#blk_1_1 ;
  int __cil_tmp255@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp27@adpcm_coder#blk_1_1 ;
  int __cil_tmp256@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp28@adpcm_coder#blk_1_1 ;
  int __cil_tmp257@adpcm_coder#blk_1_1 ;
  int __cil_tmp257@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp29@adpcm_coder#blk_1_1 ;
  int __cil_tmp258@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp30@adpcm_coder#blk_1_1 ;
  int __cil_tmp259@adpcm_coder#blk_1_1 ;
  int __cil_tmp259@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp31@adpcm_coder#blk_1_1 ;
  int __cil_tmp260@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp32@adpcm_coder#blk_1_1 ;
  int __cil_tmp261@adpcm_coder#blk_1_1 ;
  int __cil_tmp261@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp33@adpcm_coder#blk_1_1 ;
  int __cil_tmp262@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp34@adpcm_coder#blk_1_1 ;
  int __cil_tmp263@adpcm_coder#blk_1_1 ;
  int __cil_tmp263@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp35@adpcm_coder#blk_1_1 ;
  int __cil_tmp264@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp36@adpcm_coder#blk_1_1 ;
  int __cil_tmp265@adpcm_coder#blk_1_1 ;
  int __cil_tmp265@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp37@adpcm_coder#blk_1_1 ;
  int __cil_tmp266@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp38@adpcm_coder#blk_1_1 ;
  int __cil_tmp267@adpcm_coder#blk_1_1 ;
  int __cil_tmp267@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp39@adpcm_coder#blk_1_1 ;
  int __cil_tmp268@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp40@adpcm_coder#blk_1_1 ;
  int __cil_tmp269@adpcm_coder#blk_1_1 ;
  int __cil_tmp269@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp41@adpcm_coder#blk_1_1 ;
  int __cil_tmp270@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp42@adpcm_coder#blk_1_1 ;
  int __cil_tmp271@adpcm_coder#blk_1_1 ;
  int __cil_tmp271@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp43@adpcm_coder#blk_1_1 ;
  int __cil_tmp272@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp44@adpcm_coder#blk_1_1 ;
  int __cil_tmp273@adpcm_coder#blk_1_1 ;
  int __cil_tmp273@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp45@adpcm_coder#blk_1_1 ;
  int __cil_tmp274@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp46@adpcm_coder#blk_1_1 ;
  int __cil_tmp275@adpcm_coder#blk_1_1 ;
  int __cil_tmp275@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp47@adpcm_coder#blk_1_1 ;
  int __cil_tmp276@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp48@adpcm_coder#blk_1_1 ;
  int __cil_tmp277@adpcm_coder#blk_1_1 ;
  int __cil_tmp277@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp49@adpcm_coder#blk_1_1 ;
  int __cil_tmp278@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp50@adpcm_coder#blk_1_1 ;
  int __cil_tmp279@adpcm_coder#blk_1_1 ;
  int __cil_tmp279@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp51@adpcm_coder#blk_1_1 ;
  int __cil_tmp280@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp52@adpcm_coder#blk_1_1 ;
  int __cil_tmp281@adpcm_coder#blk_1_1 ;
  int __cil_tmp281@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp53@adpcm_coder#blk_1_1 ;
  int __cil_tmp282@adpcm_coder#blk_1_1[16] ;
  char *__cil_tmp54@adpcm_coder#blk_1_1 ;
  int __cil_tmp283@adpcm_coder#blk_1_1 ;
  int __cil_tmp283@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp55@adpcm_coder#blk_1_1 ;
  int __cil_tmp284@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp56@adpcm_coder#blk_1_1 ;
  int __cil_tmp285@adpcm_coder#blk_1_1 ;
  int __cil_tmp285@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp57@adpcm_coder#blk_1_1 ;
  int __cil_tmp286@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp58@adpcm_coder#blk_1_1 ;
  int __cil_tmp287@adpcm_coder#blk_1_1 ;
  int __cil_tmp287@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp59@adpcm_coder#blk_1_1 ;
  int __cil_tmp288@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp60@adpcm_coder#blk_1_1 ;
  int __cil_tmp289@adpcm_coder#blk_1_1 ;
  int __cil_tmp289@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp61@adpcm_coder#blk_1_1 ;
  int __cil_tmp290@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp62@adpcm_coder#blk_1_1 ;
  int __cil_tmp291@adpcm_coder#blk_1_1 ;
  int __cil_tmp291@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp63@adpcm_coder#blk_1_1 ;
  int __cil_tmp292@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp64@adpcm_coder#blk_1_1 ;
  int __cil_tmp293@adpcm_coder#blk_1_1 ;
  int __cil_tmp293@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp65@adpcm_coder#blk_1_1 ;
  int __cil_tmp294@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp66@adpcm_coder#blk_1_1 ;
  int __cil_tmp295@adpcm_coder#blk_1_1 ;
  int __cil_tmp295@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp67@adpcm_coder#blk_1_1 ;
  int __cil_tmp296@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp68@adpcm_coder#blk_1_1 ;
  int __cil_tmp297@adpcm_coder#blk_1_1 ;
  int __cil_tmp297@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp69@adpcm_coder#blk_1_1 ;
  int __cil_tmp298@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp70@adpcm_coder#blk_1_1 ;
  int __cil_tmp299@adpcm_coder#blk_1_1 ;
  int __cil_tmp299@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp71@adpcm_coder#blk_1_1 ;
  int __cil_tmp300@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp72@adpcm_coder#blk_1_1 ;
  int __cil_tmp301@adpcm_coder#blk_1_1 ;
  int __cil_tmp301@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp73@adpcm_coder#blk_1_1 ;
  int __cil_tmp302@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp74@adpcm_coder#blk_1_1 ;
  int __cil_tmp303@adpcm_coder#blk_1_1 ;
  int __cil_tmp303@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp75@adpcm_coder#blk_1_1 ;
  int __cil_tmp304@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp76@adpcm_coder#blk_1_1 ;
  int __cil_tmp305@adpcm_coder#blk_1_1 ;
  int __cil_tmp305@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp77@adpcm_coder#blk_1_1 ;
  int __cil_tmp306@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp78@adpcm_coder#blk_1_1 ;
  int __cil_tmp307@adpcm_coder#blk_1_1 ;
  int __cil_tmp307@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp79@adpcm_coder#blk_1_1 ;
  int __cil_tmp308@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp80@adpcm_coder#blk_1_1 ;
  int __cil_tmp309@adpcm_coder#blk_1_1 ;
  int __cil_tmp309@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp81@adpcm_coder#blk_1_1 ;
  int __cil_tmp310@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp82@adpcm_coder#blk_1_1 ;
  int __cil_tmp311@adpcm_coder#blk_1_1 ;
  int __cil_tmp311@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp83@adpcm_coder#blk_1_1 ;
  int __cil_tmp312@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp84@adpcm_coder#blk_1_1 ;
  int __cil_tmp313@adpcm_coder#blk_1_1 ;
  int __cil_tmp313@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp85@adpcm_coder#blk_1_1 ;
  int __cil_tmp314@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp86@adpcm_coder#blk_1_1 ;
  int __cil_tmp315@adpcm_coder#blk_1_1 ;
  int __cil_tmp315@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp87@adpcm_coder#blk_1_1 ;
  int __cil_tmp316@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp88@adpcm_coder#blk_1_1 ;
  int __cil_tmp317@adpcm_coder#blk_1_1 ;
  int __cil_tmp317@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp89@adpcm_coder#blk_1_1 ;
  int __cil_tmp318@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp90@adpcm_coder#blk_1_1 ;
  int __cil_tmp319@adpcm_coder#blk_1_1 ;
  int __cil_tmp319@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp91@adpcm_coder#blk_1_1 ;
  int __cil_tmp320@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp92@adpcm_coder#blk_1_1 ;
  int __cil_tmp321@adpcm_coder#blk_1_1 ;
  int __cil_tmp321@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp93@adpcm_coder#blk_1_1 ;
  int __cil_tmp322@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp94@adpcm_coder#blk_1_1 ;
  int __cil_tmp323@adpcm_coder#blk_1_1 ;
  int __cil_tmp323@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp95@adpcm_coder#blk_1_1 ;
  int __cil_tmp324@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp96@adpcm_coder#blk_1_1 ;
  int __cil_tmp325@adpcm_coder#blk_1_1 ;
  int __cil_tmp325@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp97@adpcm_coder#blk_1_1 ;
  int __cil_tmp326@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp98@adpcm_coder#blk_1_1 ;
  int __cil_tmp327@adpcm_coder#blk_1_1 ;
  int __cil_tmp327@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp99@adpcm_coder#blk_1_1 ;
  int __cil_tmp328@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp100@adpcm_coder#blk_1_1 ;
  int __cil_tmp329@adpcm_coder#blk_1_1 ;
  int __cil_tmp329@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp101@adpcm_coder#blk_1_1 ;
  int __cil_tmp330@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp102@adpcm_coder#blk_1_1 ;
  int __cil_tmp331@adpcm_coder#blk_1_1 ;
  int __cil_tmp331@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp103@adpcm_coder#blk_1_1 ;
  int __cil_tmp332@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp104@adpcm_coder#blk_1_1 ;
  int __cil_tmp333@adpcm_coder#blk_1_1 ;
  int __cil_tmp333@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp105@adpcm_coder#blk_1_1 ;
  int __cil_tmp334@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp106@adpcm_coder#blk_1_1 ;
  int __cil_tmp335@adpcm_coder#blk_1_1 ;
  int __cil_tmp335@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp107@adpcm_coder#blk_1_1 ;
  int __cil_tmp336@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp108@adpcm_coder#blk_1_1 ;
  int __cil_tmp337@adpcm_coder#blk_1_1 ;
  int __cil_tmp337@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp109@adpcm_coder#blk_1_1 ;
  int __cil_tmp338@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp110@adpcm_coder#blk_1_1 ;
  int __cil_tmp339@adpcm_coder#blk_1_1 ;
  int __cil_tmp339@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp111@adpcm_coder#blk_1_1 ;
  int __cil_tmp340@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp112@adpcm_coder#blk_1_1 ;
  int __cil_tmp341@adpcm_coder#blk_1_1 ;
  int __cil_tmp341@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp113@adpcm_coder#blk_1_1 ;
  int __cil_tmp342@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp114@adpcm_coder#blk_1_1 ;
  int __cil_tmp343@adpcm_coder#blk_1_1 ;
  int __cil_tmp343@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp115@adpcm_coder#blk_1_1 ;
  int __cil_tmp344@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp116@adpcm_coder#blk_1_1 ;
  int __cil_tmp345@adpcm_coder#blk_1_1 ;
  int __cil_tmp345@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp117@adpcm_coder#blk_1_1 ;
  int __cil_tmp346@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp118@adpcm_coder#blk_1_1 ;
  int __cil_tmp347@adpcm_coder#blk_1_1 ;
  int __cil_tmp347@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp119@adpcm_coder#blk_1_1 ;
  int __cil_tmp348@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp120@adpcm_coder#blk_1_1 ;
  int __cil_tmp349@adpcm_coder#blk_1_1 ;
  int __cil_tmp349@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp121@adpcm_coder#blk_1_1 ;
  int __cil_tmp350@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp122@adpcm_coder#blk_1_1 ;
  int __cil_tmp351@adpcm_coder#blk_1_1 ;
  int __cil_tmp351@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp123@adpcm_coder#blk_1_1 ;
  int __cil_tmp352@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp124@adpcm_coder#blk_1_1 ;
  int __cil_tmp353@adpcm_coder#blk_1_1 ;
  int __cil_tmp353@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp125@adpcm_coder#blk_1_1 ;
  int __cil_tmp354@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp126@adpcm_coder#blk_1_1 ;
  int __cil_tmp355@adpcm_coder#blk_1_1 ;
  int __cil_tmp355@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp127@adpcm_coder#blk_1_1 ;
  int __cil_tmp356@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp128@adpcm_coder#blk_1_1 ;
  int __cil_tmp357@adpcm_coder#blk_1_1 ;
  int __cil_tmp357@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp129@adpcm_coder#blk_1_1 ;
  int __cil_tmp358@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp130@adpcm_coder#blk_1_1 ;
  int __cil_tmp359@adpcm_coder#blk_1_1 ;
  int __cil_tmp359@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp131@adpcm_coder#blk_1_1 ;
  int __cil_tmp360@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp132@adpcm_coder#blk_1_1 ;
  int __cil_tmp361@adpcm_coder#blk_1_1 ;
  int __cil_tmp361@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp133@adpcm_coder#blk_1_1 ;
  int __cil_tmp362@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp134@adpcm_coder#blk_1_1 ;
  int __cil_tmp363@adpcm_coder#blk_1_1 ;
  int __cil_tmp363@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp135@adpcm_coder#blk_1_1 ;
  int __cil_tmp364@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp136@adpcm_coder#blk_1_1 ;
  int __cil_tmp365@adpcm_coder#blk_1_1 ;
  int __cil_tmp365@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp137@adpcm_coder#blk_1_1 ;
  int __cil_tmp366@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp138@adpcm_coder#blk_1_1 ;
  int __cil_tmp367@adpcm_coder#blk_1_1 ;
  int __cil_tmp367@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp139@adpcm_coder#blk_1_1 ;
  int __cil_tmp368@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp140@adpcm_coder#blk_1_1 ;
  int __cil_tmp369@adpcm_coder#blk_1_1 ;
  int __cil_tmp369@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp141@adpcm_coder#blk_1_1 ;
  int __cil_tmp370@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp142@adpcm_coder#blk_1_1 ;
  int __cil_tmp371@adpcm_coder#blk_1_1 ;
  int __cil_tmp371@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp143@adpcm_coder#blk_1_1 ;
  int __cil_tmp372@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp144@adpcm_coder#blk_1_1 ;
  int __cil_tmp373@adpcm_coder#blk_1_1 ;
  int __cil_tmp373@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp145@adpcm_coder#blk_1_1 ;
  int __cil_tmp374@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp146@adpcm_coder#blk_1_1 ;
  int __cil_tmp375@adpcm_coder#blk_1_1 ;
  int __cil_tmp375@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp147@adpcm_coder#blk_1_1 ;
  int __cil_tmp376@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp148@adpcm_coder#blk_1_1 ;
  int __cil_tmp377@adpcm_coder#blk_1_1 ;
  int __cil_tmp377@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp149@adpcm_coder#blk_1_1 ;
  int __cil_tmp378@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp150@adpcm_coder#blk_1_1 ;
  int __cil_tmp379@adpcm_coder#blk_1_1 ;
  int __cil_tmp379@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp151@adpcm_coder#blk_1_1 ;
  int __cil_tmp380@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp152@adpcm_coder#blk_1_1 ;
  int __cil_tmp381@adpcm_coder#blk_1_1 ;
  int __cil_tmp381@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp153@adpcm_coder#blk_1_1 ;
  int __cil_tmp382@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp154@adpcm_coder#blk_1_1 ;
  int __cil_tmp383@adpcm_coder#blk_1_1 ;
  int __cil_tmp383@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp155@adpcm_coder#blk_1_1 ;
  int __cil_tmp384@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp156@adpcm_coder#blk_1_1 ;
  int __cil_tmp385@adpcm_coder#blk_1_1 ;
  int __cil_tmp385@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp157@adpcm_coder#blk_1_1 ;
  int __cil_tmp386@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp158@adpcm_coder#blk_1_1 ;
  int __cil_tmp387@adpcm_coder#blk_1_1 ;
  int __cil_tmp387@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp159@adpcm_coder#blk_1_1 ;
  int __cil_tmp388@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp160@adpcm_coder#blk_1_1 ;
  int __cil_tmp389@adpcm_coder#blk_1_1 ;
  int __cil_tmp389@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp161@adpcm_coder#blk_1_1 ;
  int __cil_tmp390@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp162@adpcm_coder#blk_1_1 ;
  int __cil_tmp391@adpcm_coder#blk_1_1 ;
  int __cil_tmp391@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp163@adpcm_coder#blk_1_1 ;
  int __cil_tmp392@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp164@adpcm_coder#blk_1_1 ;
  int __cil_tmp393@adpcm_coder#blk_1_1 ;
  int __cil_tmp393@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp165@adpcm_coder#blk_1_1 ;
  int __cil_tmp394@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp166@adpcm_coder#blk_1_1 ;
  int __cil_tmp395@adpcm_coder#blk_1_1 ;
  int __cil_tmp395@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp167@adpcm_coder#blk_1_1 ;
  int __cil_tmp396@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp168@adpcm_coder#blk_1_1 ;
  int __cil_tmp397@adpcm_coder#blk_1_1 ;
  int __cil_tmp397@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp169@adpcm_coder#blk_1_1 ;
  int __cil_tmp398@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp170@adpcm_coder#blk_1_1 ;
  int __cil_tmp399@adpcm_coder#blk_1_1 ;
  int __cil_tmp399@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp171@adpcm_coder#blk_1_1 ;
  int __cil_tmp400@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp172@adpcm_coder#blk_1_1 ;
  int __cil_tmp401@adpcm_coder#blk_1_1 ;
  int __cil_tmp401@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp173@adpcm_coder#blk_1_1 ;
  int __cil_tmp402@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp174@adpcm_coder#blk_1_1 ;
  int __cil_tmp403@adpcm_coder#blk_1_1 ;
  int __cil_tmp403@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp175@adpcm_coder#blk_1_1 ;
  int __cil_tmp404@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp176@adpcm_coder#blk_1_1 ;
  int __cil_tmp405@adpcm_coder#blk_1_1 ;
  int __cil_tmp405@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp177@adpcm_coder#blk_1_1 ;
  int __cil_tmp406@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp178@adpcm_coder#blk_1_1 ;
  int __cil_tmp407@adpcm_coder#blk_1_1 ;
  int __cil_tmp407@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp179@adpcm_coder#blk_1_1 ;
  int __cil_tmp408@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp180@adpcm_coder#blk_1_1 ;
  int __cil_tmp409@adpcm_coder#blk_1_1 ;
  int __cil_tmp409@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp181@adpcm_coder#blk_1_1 ;
  int __cil_tmp410@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp182@adpcm_coder#blk_1_1 ;
  int __cil_tmp411@adpcm_coder#blk_1_1 ;
  int __cil_tmp411@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp183@adpcm_coder#blk_1_1 ;
  int __cil_tmp412@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp184@adpcm_coder#blk_1_1 ;
  int __cil_tmp413@adpcm_coder#blk_1_1 ;
  int __cil_tmp413@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp185@adpcm_coder#blk_1_1 ;
  int __cil_tmp414@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp186@adpcm_coder#blk_1_1 ;
  int __cil_tmp415@adpcm_coder#blk_1_1 ;
  int __cil_tmp415@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp187@adpcm_coder#blk_1_1 ;
  int __cil_tmp416@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp188@adpcm_coder#blk_1_1 ;
  int __cil_tmp417@adpcm_coder#blk_1_1 ;
  int __cil_tmp417@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp189@adpcm_coder#blk_1_1 ;
  int __cil_tmp418@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp190@adpcm_coder#blk_1_1 ;
  int __cil_tmp419@adpcm_coder#blk_1_1 ;
  int __cil_tmp419@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp191@adpcm_coder#blk_1_1 ;
  int __cil_tmp420@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp192@adpcm_coder#blk_1_1 ;
  int __cil_tmp421@adpcm_coder#blk_1_1 ;
  int __cil_tmp421@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp193@adpcm_coder#blk_1_1 ;
  int __cil_tmp422@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp194@adpcm_coder#blk_1_1 ;
  int __cil_tmp423@adpcm_coder#blk_1_1 ;
  int __cil_tmp423@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp195@adpcm_coder#blk_1_1 ;
  int __cil_tmp424@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp196@adpcm_coder#blk_1_1 ;
  int __cil_tmp425@adpcm_coder#blk_1_1 ;
  int __cil_tmp425@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp197@adpcm_coder#blk_1_1 ;
  int __cil_tmp426@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp198@adpcm_coder#blk_1_1 ;
  int __cil_tmp427@adpcm_coder#blk_1_1 ;
  int __cil_tmp427@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp199@adpcm_coder#blk_1_1 ;
  int __cil_tmp428@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp200@adpcm_coder#blk_1_1 ;
  int __cil_tmp429@adpcm_coder#blk_1_1 ;
  int __cil_tmp429@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp201@adpcm_coder#blk_1_1 ;
  int __cil_tmp430@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp202@adpcm_coder#blk_1_1 ;
  int __cil_tmp431@adpcm_coder#blk_1_1 ;
  int __cil_tmp431@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp203@adpcm_coder#blk_1_1 ;
  int __cil_tmp432@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp204@adpcm_coder#blk_1_1 ;
  int __cil_tmp433@adpcm_coder#blk_1_1 ;
  int __cil_tmp433@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp205@adpcm_coder#blk_1_1 ;
  int __cil_tmp434@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp206@adpcm_coder#blk_1_1 ;
  int __cil_tmp435@adpcm_coder#blk_1_1 ;
  int __cil_tmp435@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp207@adpcm_coder#blk_1_1 ;
  int __cil_tmp436@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp208@adpcm_coder#blk_1_1 ;
  int __cil_tmp437@adpcm_coder#blk_1_1 ;
  int __cil_tmp437@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp209@adpcm_coder#blk_1_1 ;
  int __cil_tmp438@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp210@adpcm_coder#blk_1_1 ;
  int __cil_tmp439@adpcm_coder#blk_1_1 ;
  int __cil_tmp439@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp211@adpcm_coder#blk_1_1 ;
  int __cil_tmp440@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp212@adpcm_coder#blk_1_1 ;
  int __cil_tmp441@adpcm_coder#blk_1_1 ;
  int __cil_tmp441@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp213@adpcm_coder#blk_1_1 ;
  int __cil_tmp442@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp214@adpcm_coder#blk_1_1 ;
  int __cil_tmp443@adpcm_coder#blk_1_1 ;
  int __cil_tmp443@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp215@adpcm_coder#blk_1_1 ;
  int __cil_tmp444@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp216@adpcm_coder#blk_1_1 ;
  int __cil_tmp445@adpcm_coder#blk_1_1 ;
  int __cil_tmp445@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp217@adpcm_coder#blk_1_1 ;
  int __cil_tmp446@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp218@adpcm_coder#blk_1_1 ;
  int __cil_tmp447@adpcm_coder#blk_1_1 ;
  int __cil_tmp447@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp219@adpcm_coder#blk_1_1 ;
  int __cil_tmp448@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp220@adpcm_coder#blk_1_1 ;
  int __cil_tmp449@adpcm_coder#blk_1_1 ;
  int __cil_tmp449@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp221@adpcm_coder#blk_1_1 ;
  int __cil_tmp450@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp222@adpcm_coder#blk_1_1 ;
  int __cil_tmp451@adpcm_coder#blk_1_1 ;
  int __cil_tmp451@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp223@adpcm_coder#blk_1_1 ;
  int __cil_tmp452@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp224@adpcm_coder#blk_1_1 ;
  int __cil_tmp453@adpcm_coder#blk_1_1 ;
  int __cil_tmp453@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp225@adpcm_coder#blk_1_1 ;
  int __cil_tmp454@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp226@adpcm_coder#blk_1_1 ;
  int __cil_tmp455@adpcm_coder#blk_1_1 ;
  int __cil_tmp455@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp227@adpcm_coder#blk_1_1 ;
  int __cil_tmp456@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp228@adpcm_coder#blk_1_1 ;
  int __cil_tmp457@adpcm_coder#blk_1_1 ;
  int __cil_tmp457@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp229@adpcm_coder#blk_1_1 ;
  int __cil_tmp458@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp230@adpcm_coder#blk_1_1 ;
  int __cil_tmp459@adpcm_coder#blk_1_1 ;
  int __cil_tmp459@adpcm_coder#blk_1_2 ;
  unsigned int __cil_tmp231@adpcm_coder#blk_1_1 ;
  int __cil_tmp460@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp232@adpcm_coder#blk_1_1 ;
  int __cil_tmp461@adpcm_coder#blk_1_1 ;
  int __cil_tmp461@adpcm_coder#blk_1_2 ;
  short __cil_tmp233@adpcm_coder#blk_1_1 ;
  char *__cil_tmp234@adpcm_coder#blk_1_1 ;
  char *__cil_tmp235@adpcm_coder#blk_1_1 ;
  char __cil_tmp236@adpcm_coder#blk_1_1 ;
  unsigned int __cil_tmp237@adpcm_coder#blk_1_1 ;
  int __cil_tmp462@adpcm_coder#blk_1_1[89] ;
  char *__cil_tmp238@adpcm_coder#blk_1_1 ;
  int step@adpcm_coder#blk_16_1 ;
  int step@adpcm_coder#blk_19_1 ;
  signed char __cil_tmp465@adpcm_coder#blk_37_1 ;
  signed char __cil_tmp466@adpcm_coder#blk_41_1 ;
  short __cil_tmp467@adpcm_coder#blk_42_1 ;
  short __cil_tmp467@adpcm_coder#blk_42_2 ;
  char *__cil_tmp248@adpcm_coder#blk_42_1 ;
  char *__cil_tmp249@adpcm_coder#blk_42_1 ;
  char __cil_tmp468@adpcm_coder#blk_42_1 ;
  char __cil_tmp468@adpcm_coder#blk_42_2 ;

  {
#line 78 "adpcm.c"
  indexTable#heapify@adpcm_coder#blk_0_1 = (int (*)[16])malloc(sizeof(int [16]));
#line 78
  stepsizeTable#heapify@adpcm_coder#blk_0_1 = (int (*)[89])malloc(sizeof(int [89]));
#line 93
  len@adpcm_coder#blk_1_1 = nsample@adpcm_coder;
#line 96
  __cil_tmp23@adpcm_coder#blk_1_1 = 0 * 4U;
#line 96
  __cil_tmp252@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp24@adpcm_coder#blk_1_1 = (char *)(__cil_tmp252@adpcm_coder#blk_1_1) + __cil_tmp23@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp253@adpcm_coder#blk_1_1 = *((int *)__cil_tmp24@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp253@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp25@adpcm_coder#blk_1_1 = 1 * 4U;
#line 96
  __cil_tmp254@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp26@adpcm_coder#blk_1_1 = (char *)(__cil_tmp254@adpcm_coder#blk_1_1) + __cil_tmp25@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp255@adpcm_coder#blk_1_1 = *((int *)__cil_tmp26@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp255@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp27@adpcm_coder#blk_1_1 = 2 * 4U;
#line 96
  __cil_tmp256@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp28@adpcm_coder#blk_1_1 = (char *)(__cil_tmp256@adpcm_coder#blk_1_1) + __cil_tmp27@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp257@adpcm_coder#blk_1_1 = *((int *)__cil_tmp28@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp257@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp29@adpcm_coder#blk_1_1 = 3 * 4U;
#line 96
  __cil_tmp258@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp30@adpcm_coder#blk_1_1 = (char *)(__cil_tmp258@adpcm_coder#blk_1_1) + __cil_tmp29@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp259@adpcm_coder#blk_1_1 = *((int *)__cil_tmp30@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp259@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp31@adpcm_coder#blk_1_1 = 4 * 4U;
#line 96
  __cil_tmp260@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp32@adpcm_coder#blk_1_1 = (char *)(__cil_tmp260@adpcm_coder#blk_1_1) + __cil_tmp31@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp261@adpcm_coder#blk_1_1 = *((int *)__cil_tmp32@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp261@adpcm_coder#blk_1_2 = 2;
#line 96
  __cil_tmp33@adpcm_coder#blk_1_1 = 5 * 4U;
#line 96
  __cil_tmp262@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp34@adpcm_coder#blk_1_1 = (char *)(__cil_tmp262@adpcm_coder#blk_1_1) + __cil_tmp33@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp263@adpcm_coder#blk_1_1 = *((int *)__cil_tmp34@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp263@adpcm_coder#blk_1_2 = 4;
#line 96
  __cil_tmp35@adpcm_coder#blk_1_1 = 6 * 4U;
#line 96
  __cil_tmp264@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp36@adpcm_coder#blk_1_1 = (char *)(__cil_tmp264@adpcm_coder#blk_1_1) + __cil_tmp35@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp265@adpcm_coder#blk_1_1 = *((int *)__cil_tmp36@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp265@adpcm_coder#blk_1_2 = 6;
#line 96
  __cil_tmp37@adpcm_coder#blk_1_1 = 7 * 4U;
#line 96
  __cil_tmp266@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp38@adpcm_coder#blk_1_1 = (char *)(__cil_tmp266@adpcm_coder#blk_1_1) + __cil_tmp37@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp267@adpcm_coder#blk_1_1 = *((int *)__cil_tmp38@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp267@adpcm_coder#blk_1_2 = 8;
#line 96
  __cil_tmp39@adpcm_coder#blk_1_1 = 8 * 4U;
#line 96
  __cil_tmp268@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp40@adpcm_coder#blk_1_1 = (char *)(__cil_tmp268@adpcm_coder#blk_1_1) + __cil_tmp39@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp269@adpcm_coder#blk_1_1 = *((int *)__cil_tmp40@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp269@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp41@adpcm_coder#blk_1_1 = 9 * 4U;
#line 96
  __cil_tmp270@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp42@adpcm_coder#blk_1_1 = (char *)(__cil_tmp270@adpcm_coder#blk_1_1) + __cil_tmp41@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp271@adpcm_coder#blk_1_1 = *((int *)__cil_tmp42@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp271@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp43@adpcm_coder#blk_1_1 = 10 * 4U;
#line 96
  __cil_tmp272@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp44@adpcm_coder#blk_1_1 = (char *)(__cil_tmp272@adpcm_coder#blk_1_1) + __cil_tmp43@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp273@adpcm_coder#blk_1_1 = *((int *)__cil_tmp44@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp273@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp45@adpcm_coder#blk_1_1 = 11 * 4U;
#line 96
  __cil_tmp274@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp46@adpcm_coder#blk_1_1 = (char *)(__cil_tmp274@adpcm_coder#blk_1_1) + __cil_tmp45@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp275@adpcm_coder#blk_1_1 = *((int *)__cil_tmp46@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp275@adpcm_coder#blk_1_2 = -1;
#line 96
  __cil_tmp47@adpcm_coder#blk_1_1 = 12 * 4U;
#line 96
  __cil_tmp276@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp48@adpcm_coder#blk_1_1 = (char *)(__cil_tmp276@adpcm_coder#blk_1_1) + __cil_tmp47@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp277@adpcm_coder#blk_1_1 = *((int *)__cil_tmp48@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp277@adpcm_coder#blk_1_2 = 2;
#line 96
  __cil_tmp49@adpcm_coder#blk_1_1 = 13 * 4U;
#line 96
  __cil_tmp278@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp50@adpcm_coder#blk_1_1 = (char *)(__cil_tmp278@adpcm_coder#blk_1_1) + __cil_tmp49@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp279@adpcm_coder#blk_1_1 = *((int *)__cil_tmp50@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp279@adpcm_coder#blk_1_2 = 4;
#line 96
  __cil_tmp51@adpcm_coder#blk_1_1 = 14 * 4U;
#line 96
  __cil_tmp280@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp52@adpcm_coder#blk_1_1 = (char *)(__cil_tmp280@adpcm_coder#blk_1_1) + __cil_tmp51@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp281@adpcm_coder#blk_1_1 = *((int *)__cil_tmp52@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp281@adpcm_coder#blk_1_2 = 6;
#line 96
  __cil_tmp53@adpcm_coder#blk_1_1 = 15 * 4U;
#line 96
  __cil_tmp282@adpcm_coder#blk_1_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 96
  __cil_tmp54@adpcm_coder#blk_1_1 = (char *)(__cil_tmp282@adpcm_coder#blk_1_1) + __cil_tmp53@adpcm_coder#blk_1_1;
#line 96
  __cil_tmp283@adpcm_coder#blk_1_1 = *((int *)__cil_tmp54@adpcm_coder#blk_1_1);
#line 96
  __cil_tmp283@adpcm_coder#blk_1_2 = 8;
#line 102
  __cil_tmp55@adpcm_coder#blk_1_1 = 0 * 4U;
#line 102
  __cil_tmp284@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp56@adpcm_coder#blk_1_1 = (char *)(__cil_tmp284@adpcm_coder#blk_1_1) + __cil_tmp55@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp285@adpcm_coder#blk_1_1 = *((int *)__cil_tmp56@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp285@adpcm_coder#blk_1_2 = 7;
#line 102
  __cil_tmp57@adpcm_coder#blk_1_1 = 1 * 4U;
#line 102
  __cil_tmp286@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp58@adpcm_coder#blk_1_1 = (char *)(__cil_tmp286@adpcm_coder#blk_1_1) + __cil_tmp57@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp287@adpcm_coder#blk_1_1 = *((int *)__cil_tmp58@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp287@adpcm_coder#blk_1_2 = 8;
#line 102
  __cil_tmp59@adpcm_coder#blk_1_1 = 2 * 4U;
#line 102
  __cil_tmp288@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp60@adpcm_coder#blk_1_1 = (char *)(__cil_tmp288@adpcm_coder#blk_1_1) + __cil_tmp59@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp289@adpcm_coder#blk_1_1 = *((int *)__cil_tmp60@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp289@adpcm_coder#blk_1_2 = 9;
#line 102
  __cil_tmp61@adpcm_coder#blk_1_1 = 3 * 4U;
#line 102
  __cil_tmp290@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp62@adpcm_coder#blk_1_1 = (char *)(__cil_tmp290@adpcm_coder#blk_1_1) + __cil_tmp61@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp291@adpcm_coder#blk_1_1 = *((int *)__cil_tmp62@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp291@adpcm_coder#blk_1_2 = 10;
#line 102
  __cil_tmp63@adpcm_coder#blk_1_1 = 4 * 4U;
#line 102
  __cil_tmp292@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp64@adpcm_coder#blk_1_1 = (char *)(__cil_tmp292@adpcm_coder#blk_1_1) + __cil_tmp63@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp293@adpcm_coder#blk_1_1 = *((int *)__cil_tmp64@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp293@adpcm_coder#blk_1_2 = 11;
#line 102
  __cil_tmp65@adpcm_coder#blk_1_1 = 5 * 4U;
#line 102
  __cil_tmp294@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp66@adpcm_coder#blk_1_1 = (char *)(__cil_tmp294@adpcm_coder#blk_1_1) + __cil_tmp65@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp295@adpcm_coder#blk_1_1 = *((int *)__cil_tmp66@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp295@adpcm_coder#blk_1_2 = 12;
#line 102
  __cil_tmp67@adpcm_coder#blk_1_1 = 6 * 4U;
#line 102
  __cil_tmp296@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp68@adpcm_coder#blk_1_1 = (char *)(__cil_tmp296@adpcm_coder#blk_1_1) + __cil_tmp67@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp297@adpcm_coder#blk_1_1 = *((int *)__cil_tmp68@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp297@adpcm_coder#blk_1_2 = 13;
#line 102
  __cil_tmp69@adpcm_coder#blk_1_1 = 7 * 4U;
#line 102
  __cil_tmp298@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp70@adpcm_coder#blk_1_1 = (char *)(__cil_tmp298@adpcm_coder#blk_1_1) + __cil_tmp69@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp299@adpcm_coder#blk_1_1 = *((int *)__cil_tmp70@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp299@adpcm_coder#blk_1_2 = 14;
#line 102
  __cil_tmp71@adpcm_coder#blk_1_1 = 8 * 4U;
#line 102
  __cil_tmp300@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp72@adpcm_coder#blk_1_1 = (char *)(__cil_tmp300@adpcm_coder#blk_1_1) + __cil_tmp71@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp301@adpcm_coder#blk_1_1 = *((int *)__cil_tmp72@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp301@adpcm_coder#blk_1_2 = 16;
#line 102
  __cil_tmp73@adpcm_coder#blk_1_1 = 9 * 4U;
#line 102
  __cil_tmp302@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp74@adpcm_coder#blk_1_1 = (char *)(__cil_tmp302@adpcm_coder#blk_1_1) + __cil_tmp73@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp303@adpcm_coder#blk_1_1 = *((int *)__cil_tmp74@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp303@adpcm_coder#blk_1_2 = 17;
#line 102
  __cil_tmp75@adpcm_coder#blk_1_1 = 10 * 4U;
#line 102
  __cil_tmp304@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp76@adpcm_coder#blk_1_1 = (char *)(__cil_tmp304@adpcm_coder#blk_1_1) + __cil_tmp75@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp305@adpcm_coder#blk_1_1 = *((int *)__cil_tmp76@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp305@adpcm_coder#blk_1_2 = 19;
#line 102
  __cil_tmp77@adpcm_coder#blk_1_1 = 11 * 4U;
#line 102
  __cil_tmp306@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp78@adpcm_coder#blk_1_1 = (char *)(__cil_tmp306@adpcm_coder#blk_1_1) + __cil_tmp77@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp307@adpcm_coder#blk_1_1 = *((int *)__cil_tmp78@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp307@adpcm_coder#blk_1_2 = 21;
#line 102
  __cil_tmp79@adpcm_coder#blk_1_1 = 12 * 4U;
#line 102
  __cil_tmp308@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp80@adpcm_coder#blk_1_1 = (char *)(__cil_tmp308@adpcm_coder#blk_1_1) + __cil_tmp79@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp309@adpcm_coder#blk_1_1 = *((int *)__cil_tmp80@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp309@adpcm_coder#blk_1_2 = 23;
#line 102
  __cil_tmp81@adpcm_coder#blk_1_1 = 13 * 4U;
#line 102
  __cil_tmp310@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp82@adpcm_coder#blk_1_1 = (char *)(__cil_tmp310@adpcm_coder#blk_1_1) + __cil_tmp81@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp311@adpcm_coder#blk_1_1 = *((int *)__cil_tmp82@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp311@adpcm_coder#blk_1_2 = 25;
#line 102
  __cil_tmp83@adpcm_coder#blk_1_1 = 14 * 4U;
#line 102
  __cil_tmp312@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp84@adpcm_coder#blk_1_1 = (char *)(__cil_tmp312@adpcm_coder#blk_1_1) + __cil_tmp83@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp313@adpcm_coder#blk_1_1 = *((int *)__cil_tmp84@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp313@adpcm_coder#blk_1_2 = 28;
#line 102
  __cil_tmp85@adpcm_coder#blk_1_1 = 15 * 4U;
#line 102
  __cil_tmp314@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp86@adpcm_coder#blk_1_1 = (char *)(__cil_tmp314@adpcm_coder#blk_1_1) + __cil_tmp85@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp315@adpcm_coder#blk_1_1 = *((int *)__cil_tmp86@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp315@adpcm_coder#blk_1_2 = 31;
#line 102
  __cil_tmp87@adpcm_coder#blk_1_1 = 16 * 4U;
#line 102
  __cil_tmp316@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp88@adpcm_coder#blk_1_1 = (char *)(__cil_tmp316@adpcm_coder#blk_1_1) + __cil_tmp87@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp317@adpcm_coder#blk_1_1 = *((int *)__cil_tmp88@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp317@adpcm_coder#blk_1_2 = 34;
#line 102
  __cil_tmp89@adpcm_coder#blk_1_1 = 17 * 4U;
#line 102
  __cil_tmp318@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp90@adpcm_coder#blk_1_1 = (char *)(__cil_tmp318@adpcm_coder#blk_1_1) + __cil_tmp89@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp319@adpcm_coder#blk_1_1 = *((int *)__cil_tmp90@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp319@adpcm_coder#blk_1_2 = 37;
#line 102
  __cil_tmp91@adpcm_coder#blk_1_1 = 18 * 4U;
#line 102
  __cil_tmp320@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp92@adpcm_coder#blk_1_1 = (char *)(__cil_tmp320@adpcm_coder#blk_1_1) + __cil_tmp91@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp321@adpcm_coder#blk_1_1 = *((int *)__cil_tmp92@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp321@adpcm_coder#blk_1_2 = 41;
#line 102
  __cil_tmp93@adpcm_coder#blk_1_1 = 19 * 4U;
#line 102
  __cil_tmp322@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp94@adpcm_coder#blk_1_1 = (char *)(__cil_tmp322@adpcm_coder#blk_1_1) + __cil_tmp93@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp323@adpcm_coder#blk_1_1 = *((int *)__cil_tmp94@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp323@adpcm_coder#blk_1_2 = 45;
#line 102
  __cil_tmp95@adpcm_coder#blk_1_1 = 20 * 4U;
#line 102
  __cil_tmp324@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp96@adpcm_coder#blk_1_1 = (char *)(__cil_tmp324@adpcm_coder#blk_1_1) + __cil_tmp95@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp325@adpcm_coder#blk_1_1 = *((int *)__cil_tmp96@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp325@adpcm_coder#blk_1_2 = 50;
#line 102
  __cil_tmp97@adpcm_coder#blk_1_1 = 21 * 4U;
#line 102
  __cil_tmp326@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp98@adpcm_coder#blk_1_1 = (char *)(__cil_tmp326@adpcm_coder#blk_1_1) + __cil_tmp97@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp327@adpcm_coder#blk_1_1 = *((int *)__cil_tmp98@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp327@adpcm_coder#blk_1_2 = 55;
#line 102
  __cil_tmp99@adpcm_coder#blk_1_1 = 22 * 4U;
#line 102
  __cil_tmp328@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp100@adpcm_coder#blk_1_1 = (char *)(__cil_tmp328@adpcm_coder#blk_1_1) + __cil_tmp99@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp329@adpcm_coder#blk_1_1 = *((int *)__cil_tmp100@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp329@adpcm_coder#blk_1_2 = 60;
#line 102
  __cil_tmp101@adpcm_coder#blk_1_1 = 23 * 4U;
#line 102
  __cil_tmp330@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp102@adpcm_coder#blk_1_1 = (char *)(__cil_tmp330@adpcm_coder#blk_1_1) + __cil_tmp101@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp331@adpcm_coder#blk_1_1 = *((int *)__cil_tmp102@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp331@adpcm_coder#blk_1_2 = 66;
#line 102
  __cil_tmp103@adpcm_coder#blk_1_1 = 24 * 4U;
#line 102
  __cil_tmp332@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp104@adpcm_coder#blk_1_1 = (char *)(__cil_tmp332@adpcm_coder#blk_1_1) + __cil_tmp103@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp333@adpcm_coder#blk_1_1 = *((int *)__cil_tmp104@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp333@adpcm_coder#blk_1_2 = 73;
#line 102
  __cil_tmp105@adpcm_coder#blk_1_1 = 25 * 4U;
#line 102
  __cil_tmp334@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp106@adpcm_coder#blk_1_1 = (char *)(__cil_tmp334@adpcm_coder#blk_1_1) + __cil_tmp105@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp335@adpcm_coder#blk_1_1 = *((int *)__cil_tmp106@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp335@adpcm_coder#blk_1_2 = 80;
#line 102
  __cil_tmp107@adpcm_coder#blk_1_1 = 26 * 4U;
#line 102
  __cil_tmp336@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp108@adpcm_coder#blk_1_1 = (char *)(__cil_tmp336@adpcm_coder#blk_1_1) + __cil_tmp107@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp337@adpcm_coder#blk_1_1 = *((int *)__cil_tmp108@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp337@adpcm_coder#blk_1_2 = 88;
#line 102
  __cil_tmp109@adpcm_coder#blk_1_1 = 27 * 4U;
#line 102
  __cil_tmp338@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp110@adpcm_coder#blk_1_1 = (char *)(__cil_tmp338@adpcm_coder#blk_1_1) + __cil_tmp109@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp339@adpcm_coder#blk_1_1 = *((int *)__cil_tmp110@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp339@adpcm_coder#blk_1_2 = 97;
#line 102
  __cil_tmp111@adpcm_coder#blk_1_1 = 28 * 4U;
#line 102
  __cil_tmp340@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp112@adpcm_coder#blk_1_1 = (char *)(__cil_tmp340@adpcm_coder#blk_1_1) + __cil_tmp111@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp341@adpcm_coder#blk_1_1 = *((int *)__cil_tmp112@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp341@adpcm_coder#blk_1_2 = 107;
#line 102
  __cil_tmp113@adpcm_coder#blk_1_1 = 29 * 4U;
#line 102
  __cil_tmp342@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp114@adpcm_coder#blk_1_1 = (char *)(__cil_tmp342@adpcm_coder#blk_1_1) + __cil_tmp113@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp343@adpcm_coder#blk_1_1 = *((int *)__cil_tmp114@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp343@adpcm_coder#blk_1_2 = 118;
#line 102
  __cil_tmp115@adpcm_coder#blk_1_1 = 30 * 4U;
#line 102
  __cil_tmp344@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp116@adpcm_coder#blk_1_1 = (char *)(__cil_tmp344@adpcm_coder#blk_1_1) + __cil_tmp115@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp345@adpcm_coder#blk_1_1 = *((int *)__cil_tmp116@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp345@adpcm_coder#blk_1_2 = 130;
#line 102
  __cil_tmp117@adpcm_coder#blk_1_1 = 31 * 4U;
#line 102
  __cil_tmp346@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp118@adpcm_coder#blk_1_1 = (char *)(__cil_tmp346@adpcm_coder#blk_1_1) + __cil_tmp117@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp347@adpcm_coder#blk_1_1 = *((int *)__cil_tmp118@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp347@adpcm_coder#blk_1_2 = 143;
#line 102
  __cil_tmp119@adpcm_coder#blk_1_1 = 32 * 4U;
#line 102
  __cil_tmp348@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp120@adpcm_coder#blk_1_1 = (char *)(__cil_tmp348@adpcm_coder#blk_1_1) + __cil_tmp119@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp349@adpcm_coder#blk_1_1 = *((int *)__cil_tmp120@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp349@adpcm_coder#blk_1_2 = 157;
#line 102
  __cil_tmp121@adpcm_coder#blk_1_1 = 33 * 4U;
#line 102
  __cil_tmp350@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp122@adpcm_coder#blk_1_1 = (char *)(__cil_tmp350@adpcm_coder#blk_1_1) + __cil_tmp121@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp351@adpcm_coder#blk_1_1 = *((int *)__cil_tmp122@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp351@adpcm_coder#blk_1_2 = 173;
#line 102
  __cil_tmp123@adpcm_coder#blk_1_1 = 34 * 4U;
#line 102
  __cil_tmp352@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp124@adpcm_coder#blk_1_1 = (char *)(__cil_tmp352@adpcm_coder#blk_1_1) + __cil_tmp123@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp353@adpcm_coder#blk_1_1 = *((int *)__cil_tmp124@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp353@adpcm_coder#blk_1_2 = 190;
#line 102
  __cil_tmp125@adpcm_coder#blk_1_1 = 35 * 4U;
#line 102
  __cil_tmp354@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp126@adpcm_coder#blk_1_1 = (char *)(__cil_tmp354@adpcm_coder#blk_1_1) + __cil_tmp125@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp355@adpcm_coder#blk_1_1 = *((int *)__cil_tmp126@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp355@adpcm_coder#blk_1_2 = 209;
#line 102
  __cil_tmp127@adpcm_coder#blk_1_1 = 36 * 4U;
#line 102
  __cil_tmp356@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp128@adpcm_coder#blk_1_1 = (char *)(__cil_tmp356@adpcm_coder#blk_1_1) + __cil_tmp127@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp357@adpcm_coder#blk_1_1 = *((int *)__cil_tmp128@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp357@adpcm_coder#blk_1_2 = 230;
#line 102
  __cil_tmp129@adpcm_coder#blk_1_1 = 37 * 4U;
#line 102
  __cil_tmp358@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp130@adpcm_coder#blk_1_1 = (char *)(__cil_tmp358@adpcm_coder#blk_1_1) + __cil_tmp129@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp359@adpcm_coder#blk_1_1 = *((int *)__cil_tmp130@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp359@adpcm_coder#blk_1_2 = 253;
#line 102
  __cil_tmp131@adpcm_coder#blk_1_1 = 38 * 4U;
#line 102
  __cil_tmp360@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp132@adpcm_coder#blk_1_1 = (char *)(__cil_tmp360@adpcm_coder#blk_1_1) + __cil_tmp131@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp361@adpcm_coder#blk_1_1 = *((int *)__cil_tmp132@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp361@adpcm_coder#blk_1_2 = 279;
#line 102
  __cil_tmp133@adpcm_coder#blk_1_1 = 39 * 4U;
#line 102
  __cil_tmp362@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp134@adpcm_coder#blk_1_1 = (char *)(__cil_tmp362@adpcm_coder#blk_1_1) + __cil_tmp133@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp363@adpcm_coder#blk_1_1 = *((int *)__cil_tmp134@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp363@adpcm_coder#blk_1_2 = 307;
#line 102
  __cil_tmp135@adpcm_coder#blk_1_1 = 40 * 4U;
#line 102
  __cil_tmp364@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp136@adpcm_coder#blk_1_1 = (char *)(__cil_tmp364@adpcm_coder#blk_1_1) + __cil_tmp135@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp365@adpcm_coder#blk_1_1 = *((int *)__cil_tmp136@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp365@adpcm_coder#blk_1_2 = 337;
#line 102
  __cil_tmp137@adpcm_coder#blk_1_1 = 41 * 4U;
#line 102
  __cil_tmp366@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp138@adpcm_coder#blk_1_1 = (char *)(__cil_tmp366@adpcm_coder#blk_1_1) + __cil_tmp137@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp367@adpcm_coder#blk_1_1 = *((int *)__cil_tmp138@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp367@adpcm_coder#blk_1_2 = 371;
#line 102
  __cil_tmp139@adpcm_coder#blk_1_1 = 42 * 4U;
#line 102
  __cil_tmp368@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp140@adpcm_coder#blk_1_1 = (char *)(__cil_tmp368@adpcm_coder#blk_1_1) + __cil_tmp139@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp369@adpcm_coder#blk_1_1 = *((int *)__cil_tmp140@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp369@adpcm_coder#blk_1_2 = 408;
#line 102
  __cil_tmp141@adpcm_coder#blk_1_1 = 43 * 4U;
#line 102
  __cil_tmp370@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp142@adpcm_coder#blk_1_1 = (char *)(__cil_tmp370@adpcm_coder#blk_1_1) + __cil_tmp141@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp371@adpcm_coder#blk_1_1 = *((int *)__cil_tmp142@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp371@adpcm_coder#blk_1_2 = 449;
#line 102
  __cil_tmp143@adpcm_coder#blk_1_1 = 44 * 4U;
#line 102
  __cil_tmp372@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp144@adpcm_coder#blk_1_1 = (char *)(__cil_tmp372@adpcm_coder#blk_1_1) + __cil_tmp143@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp373@adpcm_coder#blk_1_1 = *((int *)__cil_tmp144@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp373@adpcm_coder#blk_1_2 = 494;
#line 102
  __cil_tmp145@adpcm_coder#blk_1_1 = 45 * 4U;
#line 102
  __cil_tmp374@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp146@adpcm_coder#blk_1_1 = (char *)(__cil_tmp374@adpcm_coder#blk_1_1) + __cil_tmp145@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp375@adpcm_coder#blk_1_1 = *((int *)__cil_tmp146@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp375@adpcm_coder#blk_1_2 = 544;
#line 102
  __cil_tmp147@adpcm_coder#blk_1_1 = 46 * 4U;
#line 102
  __cil_tmp376@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp148@adpcm_coder#blk_1_1 = (char *)(__cil_tmp376@adpcm_coder#blk_1_1) + __cil_tmp147@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp377@adpcm_coder#blk_1_1 = *((int *)__cil_tmp148@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp377@adpcm_coder#blk_1_2 = 598;
#line 102
  __cil_tmp149@adpcm_coder#blk_1_1 = 47 * 4U;
#line 102
  __cil_tmp378@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp150@adpcm_coder#blk_1_1 = (char *)(__cil_tmp378@adpcm_coder#blk_1_1) + __cil_tmp149@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp379@adpcm_coder#blk_1_1 = *((int *)__cil_tmp150@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp379@adpcm_coder#blk_1_2 = 658;
#line 102
  __cil_tmp151@adpcm_coder#blk_1_1 = 48 * 4U;
#line 102
  __cil_tmp380@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp152@adpcm_coder#blk_1_1 = (char *)(__cil_tmp380@adpcm_coder#blk_1_1) + __cil_tmp151@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp381@adpcm_coder#blk_1_1 = *((int *)__cil_tmp152@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp381@adpcm_coder#blk_1_2 = 724;
#line 102
  __cil_tmp153@adpcm_coder#blk_1_1 = 49 * 4U;
#line 102
  __cil_tmp382@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp154@adpcm_coder#blk_1_1 = (char *)(__cil_tmp382@adpcm_coder#blk_1_1) + __cil_tmp153@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp383@adpcm_coder#blk_1_1 = *((int *)__cil_tmp154@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp383@adpcm_coder#blk_1_2 = 796;
#line 102
  __cil_tmp155@adpcm_coder#blk_1_1 = 50 * 4U;
#line 102
  __cil_tmp384@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp156@adpcm_coder#blk_1_1 = (char *)(__cil_tmp384@adpcm_coder#blk_1_1) + __cil_tmp155@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp385@adpcm_coder#blk_1_1 = *((int *)__cil_tmp156@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp385@adpcm_coder#blk_1_2 = 876;
#line 102
  __cil_tmp157@adpcm_coder#blk_1_1 = 51 * 4U;
#line 102
  __cil_tmp386@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp158@adpcm_coder#blk_1_1 = (char *)(__cil_tmp386@adpcm_coder#blk_1_1) + __cil_tmp157@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp387@adpcm_coder#blk_1_1 = *((int *)__cil_tmp158@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp387@adpcm_coder#blk_1_2 = 963;
#line 102
  __cil_tmp159@adpcm_coder#blk_1_1 = 52 * 4U;
#line 102
  __cil_tmp388@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp160@adpcm_coder#blk_1_1 = (char *)(__cil_tmp388@adpcm_coder#blk_1_1) + __cil_tmp159@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp389@adpcm_coder#blk_1_1 = *((int *)__cil_tmp160@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp389@adpcm_coder#blk_1_2 = 1060;
#line 102
  __cil_tmp161@adpcm_coder#blk_1_1 = 53 * 4U;
#line 102
  __cil_tmp390@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp162@adpcm_coder#blk_1_1 = (char *)(__cil_tmp390@adpcm_coder#blk_1_1) + __cil_tmp161@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp391@adpcm_coder#blk_1_1 = *((int *)__cil_tmp162@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp391@adpcm_coder#blk_1_2 = 1166;
#line 102
  __cil_tmp163@adpcm_coder#blk_1_1 = 54 * 4U;
#line 102
  __cil_tmp392@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp164@adpcm_coder#blk_1_1 = (char *)(__cil_tmp392@adpcm_coder#blk_1_1) + __cil_tmp163@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp393@adpcm_coder#blk_1_1 = *((int *)__cil_tmp164@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp393@adpcm_coder#blk_1_2 = 1282;
#line 102
  __cil_tmp165@adpcm_coder#blk_1_1 = 55 * 4U;
#line 102
  __cil_tmp394@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp166@adpcm_coder#blk_1_1 = (char *)(__cil_tmp394@adpcm_coder#blk_1_1) + __cil_tmp165@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp395@adpcm_coder#blk_1_1 = *((int *)__cil_tmp166@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp395@adpcm_coder#blk_1_2 = 1411;
#line 102
  __cil_tmp167@adpcm_coder#blk_1_1 = 56 * 4U;
#line 102
  __cil_tmp396@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp168@adpcm_coder#blk_1_1 = (char *)(__cil_tmp396@adpcm_coder#blk_1_1) + __cil_tmp167@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp397@adpcm_coder#blk_1_1 = *((int *)__cil_tmp168@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp397@adpcm_coder#blk_1_2 = 1552;
#line 102
  __cil_tmp169@adpcm_coder#blk_1_1 = 57 * 4U;
#line 102
  __cil_tmp398@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp170@adpcm_coder#blk_1_1 = (char *)(__cil_tmp398@adpcm_coder#blk_1_1) + __cil_tmp169@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp399@adpcm_coder#blk_1_1 = *((int *)__cil_tmp170@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp399@adpcm_coder#blk_1_2 = 1707;
#line 102
  __cil_tmp171@adpcm_coder#blk_1_1 = 58 * 4U;
#line 102
  __cil_tmp400@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp172@adpcm_coder#blk_1_1 = (char *)(__cil_tmp400@adpcm_coder#blk_1_1) + __cil_tmp171@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp401@adpcm_coder#blk_1_1 = *((int *)__cil_tmp172@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp401@adpcm_coder#blk_1_2 = 1878;
#line 102
  __cil_tmp173@adpcm_coder#blk_1_1 = 59 * 4U;
#line 102
  __cil_tmp402@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp174@adpcm_coder#blk_1_1 = (char *)(__cil_tmp402@adpcm_coder#blk_1_1) + __cil_tmp173@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp403@adpcm_coder#blk_1_1 = *((int *)__cil_tmp174@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp403@adpcm_coder#blk_1_2 = 2066;
#line 102
  __cil_tmp175@adpcm_coder#blk_1_1 = 60 * 4U;
#line 102
  __cil_tmp404@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp176@adpcm_coder#blk_1_1 = (char *)(__cil_tmp404@adpcm_coder#blk_1_1) + __cil_tmp175@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp405@adpcm_coder#blk_1_1 = *((int *)__cil_tmp176@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp405@adpcm_coder#blk_1_2 = 2272;
#line 102
  __cil_tmp177@adpcm_coder#blk_1_1 = 61 * 4U;
#line 102
  __cil_tmp406@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp178@adpcm_coder#blk_1_1 = (char *)(__cil_tmp406@adpcm_coder#blk_1_1) + __cil_tmp177@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp407@adpcm_coder#blk_1_1 = *((int *)__cil_tmp178@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp407@adpcm_coder#blk_1_2 = 2499;
#line 102
  __cil_tmp179@adpcm_coder#blk_1_1 = 62 * 4U;
#line 102
  __cil_tmp408@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp180@adpcm_coder#blk_1_1 = (char *)(__cil_tmp408@adpcm_coder#blk_1_1) + __cil_tmp179@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp409@adpcm_coder#blk_1_1 = *((int *)__cil_tmp180@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp409@adpcm_coder#blk_1_2 = 2749;
#line 102
  __cil_tmp181@adpcm_coder#blk_1_1 = 63 * 4U;
#line 102
  __cil_tmp410@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp182@adpcm_coder#blk_1_1 = (char *)(__cil_tmp410@adpcm_coder#blk_1_1) + __cil_tmp181@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp411@adpcm_coder#blk_1_1 = *((int *)__cil_tmp182@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp411@adpcm_coder#blk_1_2 = 3024;
#line 102
  __cil_tmp183@adpcm_coder#blk_1_1 = 64 * 4U;
#line 102
  __cil_tmp412@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp184@adpcm_coder#blk_1_1 = (char *)(__cil_tmp412@adpcm_coder#blk_1_1) + __cil_tmp183@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp413@adpcm_coder#blk_1_1 = *((int *)__cil_tmp184@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp413@adpcm_coder#blk_1_2 = 3327;
#line 102
  __cil_tmp185@adpcm_coder#blk_1_1 = 65 * 4U;
#line 102
  __cil_tmp414@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp186@adpcm_coder#blk_1_1 = (char *)(__cil_tmp414@adpcm_coder#blk_1_1) + __cil_tmp185@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp415@adpcm_coder#blk_1_1 = *((int *)__cil_tmp186@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp415@adpcm_coder#blk_1_2 = 3660;
#line 102
  __cil_tmp187@adpcm_coder#blk_1_1 = 66 * 4U;
#line 102
  __cil_tmp416@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp188@adpcm_coder#blk_1_1 = (char *)(__cil_tmp416@adpcm_coder#blk_1_1) + __cil_tmp187@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp417@adpcm_coder#blk_1_1 = *((int *)__cil_tmp188@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp417@adpcm_coder#blk_1_2 = 4026;
#line 102
  __cil_tmp189@adpcm_coder#blk_1_1 = 67 * 4U;
#line 102
  __cil_tmp418@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp190@adpcm_coder#blk_1_1 = (char *)(__cil_tmp418@adpcm_coder#blk_1_1) + __cil_tmp189@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp419@adpcm_coder#blk_1_1 = *((int *)__cil_tmp190@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp419@adpcm_coder#blk_1_2 = 4428;
#line 102
  __cil_tmp191@adpcm_coder#blk_1_1 = 68 * 4U;
#line 102
  __cil_tmp420@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp192@adpcm_coder#blk_1_1 = (char *)(__cil_tmp420@adpcm_coder#blk_1_1) + __cil_tmp191@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp421@adpcm_coder#blk_1_1 = *((int *)__cil_tmp192@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp421@adpcm_coder#blk_1_2 = 4871;
#line 102
  __cil_tmp193@adpcm_coder#blk_1_1 = 69 * 4U;
#line 102
  __cil_tmp422@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp194@adpcm_coder#blk_1_1 = (char *)(__cil_tmp422@adpcm_coder#blk_1_1) + __cil_tmp193@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp423@adpcm_coder#blk_1_1 = *((int *)__cil_tmp194@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp423@adpcm_coder#blk_1_2 = 5358;
#line 102
  __cil_tmp195@adpcm_coder#blk_1_1 = 70 * 4U;
#line 102
  __cil_tmp424@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp196@adpcm_coder#blk_1_1 = (char *)(__cil_tmp424@adpcm_coder#blk_1_1) + __cil_tmp195@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp425@adpcm_coder#blk_1_1 = *((int *)__cil_tmp196@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp425@adpcm_coder#blk_1_2 = 5894;
#line 102
  __cil_tmp197@adpcm_coder#blk_1_1 = 71 * 4U;
#line 102
  __cil_tmp426@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp198@adpcm_coder#blk_1_1 = (char *)(__cil_tmp426@adpcm_coder#blk_1_1) + __cil_tmp197@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp427@adpcm_coder#blk_1_1 = *((int *)__cil_tmp198@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp427@adpcm_coder#blk_1_2 = 6484;
#line 102
  __cil_tmp199@adpcm_coder#blk_1_1 = 72 * 4U;
#line 102
  __cil_tmp428@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp200@adpcm_coder#blk_1_1 = (char *)(__cil_tmp428@adpcm_coder#blk_1_1) + __cil_tmp199@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp429@adpcm_coder#blk_1_1 = *((int *)__cil_tmp200@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp429@adpcm_coder#blk_1_2 = 7132;
#line 102
  __cil_tmp201@adpcm_coder#blk_1_1 = 73 * 4U;
#line 102
  __cil_tmp430@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp202@adpcm_coder#blk_1_1 = (char *)(__cil_tmp430@adpcm_coder#blk_1_1) + __cil_tmp201@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp431@adpcm_coder#blk_1_1 = *((int *)__cil_tmp202@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp431@adpcm_coder#blk_1_2 = 7845;
#line 102
  __cil_tmp203@adpcm_coder#blk_1_1 = 74 * 4U;
#line 102
  __cil_tmp432@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp204@adpcm_coder#blk_1_1 = (char *)(__cil_tmp432@adpcm_coder#blk_1_1) + __cil_tmp203@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp433@adpcm_coder#blk_1_1 = *((int *)__cil_tmp204@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp433@adpcm_coder#blk_1_2 = 8630;
#line 102
  __cil_tmp205@adpcm_coder#blk_1_1 = 75 * 4U;
#line 102
  __cil_tmp434@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp206@adpcm_coder#blk_1_1 = (char *)(__cil_tmp434@adpcm_coder#blk_1_1) + __cil_tmp205@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp435@adpcm_coder#blk_1_1 = *((int *)__cil_tmp206@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp435@adpcm_coder#blk_1_2 = 9493;
#line 102
  __cil_tmp207@adpcm_coder#blk_1_1 = 76 * 4U;
#line 102
  __cil_tmp436@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp208@adpcm_coder#blk_1_1 = (char *)(__cil_tmp436@adpcm_coder#blk_1_1) + __cil_tmp207@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp437@adpcm_coder#blk_1_1 = *((int *)__cil_tmp208@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp437@adpcm_coder#blk_1_2 = 10442;
#line 102
  __cil_tmp209@adpcm_coder#blk_1_1 = 77 * 4U;
#line 102
  __cil_tmp438@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp210@adpcm_coder#blk_1_1 = (char *)(__cil_tmp438@adpcm_coder#blk_1_1) + __cil_tmp209@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp439@adpcm_coder#blk_1_1 = *((int *)__cil_tmp210@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp439@adpcm_coder#blk_1_2 = 11487;
#line 102
  __cil_tmp211@adpcm_coder#blk_1_1 = 78 * 4U;
#line 102
  __cil_tmp440@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp212@adpcm_coder#blk_1_1 = (char *)(__cil_tmp440@adpcm_coder#blk_1_1) + __cil_tmp211@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp441@adpcm_coder#blk_1_1 = *((int *)__cil_tmp212@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp441@adpcm_coder#blk_1_2 = 12635;
#line 102
  __cil_tmp213@adpcm_coder#blk_1_1 = 79 * 4U;
#line 102
  __cil_tmp442@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp214@adpcm_coder#blk_1_1 = (char *)(__cil_tmp442@adpcm_coder#blk_1_1) + __cil_tmp213@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp443@adpcm_coder#blk_1_1 = *((int *)__cil_tmp214@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp443@adpcm_coder#blk_1_2 = 13899;
#line 102
  __cil_tmp215@adpcm_coder#blk_1_1 = 80 * 4U;
#line 102
  __cil_tmp444@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp216@adpcm_coder#blk_1_1 = (char *)(__cil_tmp444@adpcm_coder#blk_1_1) + __cil_tmp215@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp445@adpcm_coder#blk_1_1 = *((int *)__cil_tmp216@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp445@adpcm_coder#blk_1_2 = 15289;
#line 102
  __cil_tmp217@adpcm_coder#blk_1_1 = 81 * 4U;
#line 102
  __cil_tmp446@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp218@adpcm_coder#blk_1_1 = (char *)(__cil_tmp446@adpcm_coder#blk_1_1) + __cil_tmp217@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp447@adpcm_coder#blk_1_1 = *((int *)__cil_tmp218@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp447@adpcm_coder#blk_1_2 = 16818;
#line 102
  __cil_tmp219@adpcm_coder#blk_1_1 = 82 * 4U;
#line 102
  __cil_tmp448@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp220@adpcm_coder#blk_1_1 = (char *)(__cil_tmp448@adpcm_coder#blk_1_1) + __cil_tmp219@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp449@adpcm_coder#blk_1_1 = *((int *)__cil_tmp220@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp449@adpcm_coder#blk_1_2 = 18500;
#line 102
  __cil_tmp221@adpcm_coder#blk_1_1 = 83 * 4U;
#line 102
  __cil_tmp450@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp222@adpcm_coder#blk_1_1 = (char *)(__cil_tmp450@adpcm_coder#blk_1_1) + __cil_tmp221@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp451@adpcm_coder#blk_1_1 = *((int *)__cil_tmp222@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp451@adpcm_coder#blk_1_2 = 20350;
#line 102
  __cil_tmp223@adpcm_coder#blk_1_1 = 84 * 4U;
#line 102
  __cil_tmp452@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp224@adpcm_coder#blk_1_1 = (char *)(__cil_tmp452@adpcm_coder#blk_1_1) + __cil_tmp223@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp453@adpcm_coder#blk_1_1 = *((int *)__cil_tmp224@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp453@adpcm_coder#blk_1_2 = 22385;
#line 102
  __cil_tmp225@adpcm_coder#blk_1_1 = 85 * 4U;
#line 102
  __cil_tmp454@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp226@adpcm_coder#blk_1_1 = (char *)(__cil_tmp454@adpcm_coder#blk_1_1) + __cil_tmp225@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp455@adpcm_coder#blk_1_1 = *((int *)__cil_tmp226@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp455@adpcm_coder#blk_1_2 = 24623;
#line 102
  __cil_tmp227@adpcm_coder#blk_1_1 = 86 * 4U;
#line 102
  __cil_tmp456@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp228@adpcm_coder#blk_1_1 = (char *)(__cil_tmp456@adpcm_coder#blk_1_1) + __cil_tmp227@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp457@adpcm_coder#blk_1_1 = *((int *)__cil_tmp228@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp457@adpcm_coder#blk_1_2 = 27086;
#line 102
  __cil_tmp229@adpcm_coder#blk_1_1 = 87 * 4U;
#line 102
  __cil_tmp458@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp230@adpcm_coder#blk_1_1 = (char *)(__cil_tmp458@adpcm_coder#blk_1_1) + __cil_tmp229@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp459@adpcm_coder#blk_1_1 = *((int *)__cil_tmp230@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp459@adpcm_coder#blk_1_2 = 29794;
#line 102
  __cil_tmp231@adpcm_coder#blk_1_1 = 88 * 4U;
#line 102
  __cil_tmp460@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 102
  __cil_tmp232@adpcm_coder#blk_1_1 = (char *)(__cil_tmp460@adpcm_coder#blk_1_1) + __cil_tmp231@adpcm_coder#blk_1_1;
#line 102
  __cil_tmp461@adpcm_coder#blk_1_1 = *((int *)__cil_tmp232@adpcm_coder#blk_1_1);
#line 102
  __cil_tmp461@adpcm_coder#blk_1_2 = 32767;
#line 114
  outp@adpcm_coder#blk_1_1 = (signed char *)outdata@adpcm_coder;
#line 115
  inp@adpcm_coder#blk_1_1 = indata@adpcm_coder;
#line 117
  __cil_tmp233@adpcm_coder#blk_1_1 = *((short *)state@adpcm_coder);
#line 117
  valpred@adpcm_coder#blk_1_1 = (int )__cil_tmp233@adpcm_coder#blk_1_1;
#line 118
  __cil_tmp234@adpcm_coder#blk_1_1 = (char *)state@adpcm_coder;
#line 118
  __cil_tmp235@adpcm_coder#blk_1_1 = __cil_tmp234@adpcm_coder#blk_1_1 + 2;
#line 118
  __cil_tmp236@adpcm_coder#blk_1_1 = *__cil_tmp235@adpcm_coder#blk_1_1;
#line 118
  index@adpcm_coder#blk_1_1 = (int )__cil_tmp236@adpcm_coder#blk_1_1;
#line 119
  __cil_tmp237@adpcm_coder#blk_1_1 = index@adpcm_coder#blk_1_1 * 4U;
#line 119
  __cil_tmp462@adpcm_coder#blk_1_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 119
  __cil_tmp238@adpcm_coder#blk_1_1 = (char *)(__cil_tmp462@adpcm_coder#blk_1_1) + __cil_tmp237@adpcm_coder#blk_1_1;
#line 119
  step@adpcm_coder#blk_1_1 = *((int *)__cil_tmp238@adpcm_coder#blk_1_1);
#line 121
  bufferstep@adpcm_coder#blk_1_1 = 1;
  {
#line 123
  while (1) {
    while_0_continue: /* CIL Label */ ;
#line 123
    if (! (len@adpcm_coder#phi_4 > 0)) {
      goto while_0_break;
    }
#line 124
    tmp@adpcm_coder#blk_7_1 = inp@adpcm_coder#phi_4;
#line 124
    inp@adpcm_coder#blk_7_1 = inp@adpcm_coder#phi_4 + 1;
#line 124
    __cil_tmp239@adpcm_coder#blk_7_1 = *tmp@adpcm_coder#blk_7_1;
#line 124
    val@adpcm_coder#blk_7_1 = (int )__cil_tmp239@adpcm_coder#blk_7_1;
#line 127
    diff@adpcm_coder#blk_7_1 = val@adpcm_coder#blk_7_1 - valpred@adpcm_coder#phi_4;
#line 128
    if (diff@adpcm_coder#blk_7_1 < 0) {
#line 128
      sign@adpcm_coder#blk_9_1 = 8;
    } else {
#line 128
      sign@adpcm_coder#blk_10_1 = 0;
    }
#line 129
    if (sign@adpcm_coder#phi_11) {
#line 129
      diff@adpcm_coder#blk_12_1 = - diff@adpcm_coder#blk_7_1;
    }
#line 140
    delta@adpcm_coder#blk_13_1 = 0;
#line 141
    vpdiff@adpcm_coder#blk_13_1 = step@adpcm_coder#phi_4 >> 3;
#line 143
    if (diff@adpcm_coder#phi_13 >= step@adpcm_coder#phi_4) {
#line 144
      delta@adpcm_coder#blk_15_1 = 4;
#line 145
      diff@adpcm_coder#blk_15_1 = diff@adpcm_coder#phi_13 - step@adpcm_coder#phi_4;
#line 146
      vpdiff@adpcm_coder#blk_15_1 = vpdiff@adpcm_coder#blk_13_1 + step@adpcm_coder#phi_4;
    }
#line 148
    step@adpcm_coder#blk_16_1 = step@adpcm_coder#phi_4 >> 1;
#line 149
    if (diff@adpcm_coder#phi_16 >= step@adpcm_coder#blk_16_1) {
#line 150
      delta@adpcm_coder#blk_18_1 = delta@adpcm_coder#phi_16 | 2;
#line 151
      diff@adpcm_coder#blk_18_1 = diff@adpcm_coder#phi_16 - step@adpcm_coder#blk_16_1;
#line 152
      vpdiff@adpcm_coder#blk_18_1 = vpdiff@adpcm_coder#phi_16 + step@adpcm_coder#blk_16_1;
    }
#line 154
    step@adpcm_coder#blk_19_1 = step@adpcm_coder#blk_16_1 >> 1;
#line 155
    if (diff@adpcm_coder#phi_19 >= step@adpcm_coder#blk_19_1) {
#line 156
      delta@adpcm_coder#blk_21_1 = delta@adpcm_coder#phi_19 | 1;
#line 157
      vpdiff@adpcm_coder#blk_21_1 = vpdiff@adpcm_coder#phi_19 + step@adpcm_coder#blk_19_1;
    }
#line 161
    if (sign@adpcm_coder#phi_11) {
#line 162
      valpred@adpcm_coder#blk_23_1 = valpred@adpcm_coder#phi_4 - vpdiff@adpcm_coder#phi_22;
    } else {
#line 164
      valpred@adpcm_coder#blk_24_1 = valpred@adpcm_coder#phi_4 + vpdiff@adpcm_coder#phi_22;
    }
#line 167
    if (valpred@adpcm_coder#phi_25 > 32767) {
#line 168
      valpred@adpcm_coder#blk_26_1 = 32767;
    } else {
#line 169
      if (valpred@adpcm_coder#phi_25 < -32768) {
#line 170
        valpred@adpcm_coder#blk_28_1 = -32768;
      }
    }
#line 173
    delta@adpcm_coder#blk_29_1 = delta@adpcm_coder#phi_22 | sign@adpcm_coder#phi_11;
#line 175
    __cil_tmp240@adpcm_coder#blk_29_1 = delta@adpcm_coder#blk_29_1 * 4U;
#line 175
    __cil_tmp463@adpcm_coder#blk_29_1 = *indexTable#heapify@adpcm_coder#blk_0_1;
#line 175
    __cil_tmp241@adpcm_coder#blk_29_1 = (char *)(__cil_tmp463@adpcm_coder#blk_29_1) + __cil_tmp240@adpcm_coder#blk_29_1;
#line 175
    __cil_tmp242@adpcm_coder#blk_29_1 = *((int *)__cil_tmp241@adpcm_coder#blk_29_1);
#line 175
    index@adpcm_coder#blk_29_1 = index@adpcm_coder#phi_4 + __cil_tmp242@adpcm_coder#blk_29_1;
#line 176
    if (index@adpcm_coder#blk_29_1 < 0) {
#line 176
      index@adpcm_coder#blk_31_1 = 0;
    }
#line 177
    if (index@adpcm_coder#phi_32 > 88) {
#line 177
      index@adpcm_coder#blk_33_1 = 88;
    }
#line 178
    __cil_tmp243@adpcm_coder#blk_34_1 = index@adpcm_coder#phi_34 * 4U;
#line 178
    __cil_tmp464@adpcm_coder#blk_34_1 = *stepsizeTable#heapify@adpcm_coder#blk_0_1;
#line 178
    __cil_tmp244@adpcm_coder#blk_34_1 = (char *)(__cil_tmp464@adpcm_coder#blk_34_1) + __cil_tmp243@adpcm_coder#blk_34_1;
#line 178
    step@adpcm_coder#blk_34_1 = *((int *)__cil_tmp244@adpcm_coder#blk_34_1);
#line 181
    if (bufferstep@adpcm_coder#phi_4) {
#line 182
      __cil_tmp245@adpcm_coder#blk_36_1 = delta@adpcm_coder#blk_29_1 << 4;
#line 182
      outputbuffer@adpcm_coder#blk_36_1 = __cil_tmp245@adpcm_coder#blk_36_1 & 240;
    } else {
#line 184
      tmp___0@adpcm_coder#blk_37_1 = outp@adpcm_coder#phi_4;
#line 184
      outp@adpcm_coder#blk_37_1 = outp@adpcm_coder#phi_4 + 1;
#line 184
      __cil_tmp246@adpcm_coder#blk_37_1 = delta@adpcm_coder#blk_29_1 & 15;
#line 184
      __cil_tmp247@adpcm_coder#blk_37_1 = __cil_tmp246@adpcm_coder#blk_37_1 | outputbuffer@adpcm_coder#phi_4;
#line 184
      __cil_tmp465@adpcm_coder#blk_37_1 = *tmp___0@adpcm_coder#blk_37_1;
#line 184
      __cil_tmp465@adpcm_coder#blk_37_2 = (signed char )__cil_tmp247@adpcm_coder#blk_37_1;
    }
#line 186
    bufferstep@adpcm_coder#blk_38_1 = ! bufferstep@adpcm_coder#phi_4;
#line 123
    len@adpcm_coder#blk_38_1 = len@adpcm_coder#phi_4 - 1;
  }
  while_0_break: /* CIL Label */ ;
  }
#line 190
  if (! bufferstep@adpcm_coder#phi_4) {
#line 191
    tmp___1@adpcm_coder#blk_41_1 = outp@adpcm_coder#phi_4;
#line 191
    outp@adpcm_coder#blk_41_1 = outp@adpcm_coder#phi_4 + 1;
#line 191
    __cil_tmp466@adpcm_coder#blk_41_1 = *tmp___1@adpcm_coder#blk_41_1;
#line 191
    __cil_tmp466@adpcm_coder#blk_41_2 = (signed char )outputbuffer@adpcm_coder#phi_4;
  }
#line 193
  __cil_tmp467@adpcm_coder#blk_42_1 = *((short *)state@adpcm_coder);
#line 193
  __cil_tmp467@adpcm_coder#blk_42_2 = (short )valpred@adpcm_coder#phi_4;
#line 194
  __cil_tmp248@adpcm_coder#blk_42_1 = (char *)state@adpcm_coder;
#line 194
  __cil_tmp249@adpcm_coder#blk_42_1 = __cil_tmp248@adpcm_coder#blk_42_1 + 2;
#line 194
  __cil_tmp468@adpcm_coder#blk_42_1 = *__cil_tmp249@adpcm_coder#blk_42_1;
#line 194
  __cil_tmp468@adpcm_coder#blk_42_2 = (char )index@adpcm_coder#phi_4;
#line 195
  return;
}
}
